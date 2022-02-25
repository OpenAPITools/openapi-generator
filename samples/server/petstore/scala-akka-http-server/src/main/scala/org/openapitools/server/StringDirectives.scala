package org.openapitools.server

import akka.http.scaladsl.common._
import akka.http.scaladsl.server.{Directive, Directive0, Directive1, InvalidRequiredValueForQueryParamRejection, MalformedFormFieldRejection, MissingFormFieldRejection, MissingQueryParamRejection, UnsupportedRequestContentTypeRejection}
import akka.http.scaladsl.server.directives.BasicDirectives
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait StringDirectives {
  implicit def _symbol2NR(symbol: Symbol): NameReceptacle[String] = new NameReceptacle[String](symbol.name)
  implicit def _string2NR(string: String): NameReceptacle[String] = new NameReceptacle[String](string)

  import StringDirectives._
  type StringValueProvider = Map[String, String]

  def stringField(pdm: StringMagnet): pdm.Out = pdm()

  def stringFields(pdm: StringMagnet): pdm.Out = pdm()

}

object StringDirectives extends StringDirectives {

  sealed trait StringMagnet {
    type Out
    def apply(): Out
  }
  object StringMagnet {
    implicit def apply[T](value: T)(implicit sdef: StringDef[T]): StringMagnet { type Out = sdef.Out } =
      new StringMagnet {
        type Out = sdef.Out
        def apply(): sdef.Out = sdef(value)
      }
  }

  type StringDefAux[A, B] = StringDef[A] { type Out = B }
  sealed trait StringDef[T] {
    type Out
    def apply(value: T): Out
  }
  object StringDef {
    protected def stringDef[A, B](f: A => B): StringDefAux[A, B] =
      new StringDef[A] {
        type Out = B

        def apply(value: A): B = f(value)
      }

    import akka.http.scaladsl.server.directives.BasicDirectives._
    import akka.http.scaladsl.server.directives.FutureDirectives._
    import akka.http.scaladsl.server.directives.RouteDirectives._
    import akka.http.scaladsl.unmarshalling._

    type FSU[T] = FromStringUnmarshaller[T]
    type FSOU[T] = Unmarshaller[Option[String], T]
    type SFVP = StringValueProvider

    protected def extractField[A, B](f: A => Directive1[B]): StringDefAux[A, Directive1[B]] = stringDef(f)

    protected def handleFieldResult[T](fieldName: String, result: Future[T]): Directive1[T] = onComplete(result).flatMap {
      case Success(x) => provide(x)
      case Failure(Unmarshaller.NoContentException) => reject(MissingFormFieldRejection(fieldName))
      case Failure(x: UnsupportedContentTypeException) => reject(UnsupportedRequestContentTypeRejection(x.supported, x.actualContentType))
      case Failure(x) => reject(MalformedFormFieldRejection(fieldName, if (x.getMessage == null) "" else x.getMessage, Option(x.getCause)))
    }

    private def filter[T](paramName: String, fsou: FSOU[T])(implicit vp: SFVP): Directive1[T] = {
      extract { ctx =>
        import ctx.{executionContext, materializer}
        handleFieldResult(paramName, fsou(vp.get(paramName)))
      }.flatMap(identity)
    }

    implicit def forString(implicit fsu: FSU[String], vp: SFVP): StringDefAux[String, Directive1[String]] =
      extractField[String, String] { string => filter(string, fsu) }
    implicit def forSymbol(implicit fsu: FSU[String], vp: SFVP): StringDefAux[Symbol, Directive1[String]] =
      extractField[Symbol, String] { symbol => filter(symbol.name, fsu) }
    implicit def forNR[T](implicit fsu: FSU[T], vp: SFVP): StringDefAux[NameReceptacle[T], Directive1[T]] =
      extractField[NameReceptacle[T], T] { nr => filter(nr.name, fsu) }
    implicit def forNUR[T](implicit vp: SFVP): StringDefAux[NameUnmarshallerReceptacle[T], Directive1[T]] =
      extractField[NameUnmarshallerReceptacle[T], T] { nr => filter(nr.name, nr.um) }
    implicit def forNOR[T](implicit fsou: FSOU[T], vp: SFVP): StringDefAux[NameOptionReceptacle[T], Directive1[Option[T]]] =
      extractField[NameOptionReceptacle[T], Option[T]] { nr => filter[Option[T]](nr.name, fsou) }
    implicit def forNDR[T](implicit fsou: FSOU[T], vp: SFVP): StringDefAux[NameDefaultReceptacle[T], Directive1[T]] =
      extractField[NameDefaultReceptacle[T], T] { nr => filter[T](nr.name, fsou withDefaultValue nr.default) }
    implicit def forNOUR[T](implicit vp: SFVP): StringDefAux[NameOptionUnmarshallerReceptacle[T], Directive1[Option[T]]] =
      extractField[NameOptionUnmarshallerReceptacle[T], Option[T]] { nr => filter(nr.name, nr.um: FSOU[T]) }
    implicit def forNDUR[T](implicit vp: SFVP): StringDefAux[NameDefaultUnmarshallerReceptacle[T], Directive1[T]] =
      extractField[NameDefaultUnmarshallerReceptacle[T], T] { nr => filter[T](nr.name, (nr.um: FSOU[T]) withDefaultValue nr.default) }

    //////////////////// required parameter support ////////////////////

    private def requiredFilter[T](paramName: String, fsou: FSOU[T], requiredValue: Any)(implicit vp: SFVP): Directive0 = {
      extract { ctx =>
        import ctx.{executionContext, materializer}
        onComplete(fsou(vp.get(paramName))) flatMap {
          case Success(value) if value == requiredValue => pass
          case Success(value) => reject(InvalidRequiredValueForQueryParamRejection(paramName, requiredValue.toString, value.toString)).toDirective[Unit]
          case _ => reject(MissingQueryParamRejection(paramName)).toDirective[Unit]
        }
      }.flatMap(identity)
    }

    implicit def forRVR[T](implicit fsu: FSU[T], vp: SFVP): StringDefAux[RequiredValueReceptacle[T], Directive0] =
      stringDef[RequiredValueReceptacle[T], Directive0] { rvr => requiredFilter(rvr.name, fsu, rvr.requiredValue) }

    implicit def forRVDR[T](implicit vp: SFVP): StringDefAux[RequiredValueUnmarshallerReceptacle[T], Directive0] =
      stringDef[RequiredValueUnmarshallerReceptacle[T], Directive0] { rvr => requiredFilter(rvr.name, rvr.um, rvr.requiredValue) }

    //////////////////// tuple support ////////////////////

    import akka.http.scaladsl.server.util.BinaryPolyFunc
    import akka.http.scaladsl.server.util.TupleOps._

    implicit def forTuple[T](implicit fold: FoldLeft[Directive0, T, ConvertStringDefAndConcatenate.type]): StringDefAux[T, fold.Out] =
      stringDef[T, fold.Out](fold(BasicDirectives.pass, _))

    object ConvertStringDefAndConcatenate extends BinaryPolyFunc {
      implicit def from[P, TA, TB](implicit sdef: StringDef[P] {type Out = Directive[TB]}, ev: Join[TA, TB]): BinaryPolyFunc.Case[Directive[TA], P, ConvertStringDefAndConcatenate.type] {type Out = Directive[ev.Out]} =
        at[Directive[TA], P] { (a, t) => a & sdef(t) }
    }

  }
}
