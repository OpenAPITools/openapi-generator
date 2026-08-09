import 'reflect-metadata'
import Koa from 'koa'
import { Container } from 'typedi'
import { routingConfigs } from './routing.options'
import { useMiddlewares } from './koa.middlewares'
import { useKoaServer, useContainer } from 'routing-controllers'

const createServer = async (): Promise<Koa> => {
  const koa: Koa = new Koa()

  useMiddlewares(koa)

  useContainer(Container)

  const app: Koa = useKoaServer<Koa>(koa, routingConfigs)

  return app
}

export default createServer
