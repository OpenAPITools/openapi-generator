def openapi_generator_repositories(openapi_generator_cli_version = "4.1.0", prefix = "io_bazel_rules_openapi_generator"):
    native.maven_jar(
        name = prefix + "_cli",
        artifact = "org.openapitools:openapi-generator-cli:" + openapi_generator_cli_version,
    )
    native.bind(
        name = prefix + "/dependency/openapi-generator-cli",
        actual = "@" + prefix + "_cli//jar",
    )

def _comma_separated_pairs(pairs):
    return ",".join([
        "{}={}".format(k, v)
        for k, v in pairs.items()
    ])

def _new_generator_command(ctx, declared_dir, rjars):
    java_path = ctx.attr._jdk[java_common.JavaRuntimeInfo].java_executable_exec_path
    gen_cmd = str(java_path)

    gen_cmd += " -cp {cli_jar}:{jars} org.openapitools.codegen.OpenAPIGenerator generate -i {spec} -g {generator} -o {output}".format(
        java = java_path,
        cli_jar = ctx.file.openapi_generator_cli.path,
        jars = ":".join([j.path for j in rjars.to_list()]),
        spec = ctx.file.spec.path,
        generator = ctx.attr.generator,
        output = declared_dir.path,
    )

    gen_cmd += ' -p "{properties}"'.format(
        properties = _comma_separated_pairs(ctx.attr.system_properties),
    )

    additional_properties = dict(ctx.attr.additional_properties)

    # This is needed to ensure reproducible Java output
    if ctx.attr.generator == "java" and \
       "hideGenerationTimestamp" not in ctx.attr.additional_properties:
        additional_properties["hideGenerationTimestamp"] = "true"

    gen_cmd += ' --additional-properties "{properties}"'.format(
        properties = _comma_separated_pairs(additional_properties),
    )

    gen_cmd += ' --type-mappings "{mappings}"'.format(
        mappings = _comma_separated_pairs(ctx.attr.type_mappings),
    )

    if ctx.attr.api_package:
        gen_cmd += " --api-package {package}".format(
            package = ctx.attr.api_package,
        )
    if ctx.attr.invoker_package:
        gen_cmd += " --invoker-package {package}".format(
            package = ctx.attr.invoker_package,
        )
    if ctx.attr.model_package:
        gen_cmd += " --model-package {package}".format(
            package = ctx.attr.model_package,
        )

    # fixme: by default, openapi-generator is rather verbose. this helps with that but can also mask useful error messages
    # when it fails. look into log configuration options. it's a java app so perhaps just a log4j.properties or something
    gen_cmd += " 1>/dev/null"
    return gen_cmd

def _impl(ctx):
    jars = _collect_jars(ctx.attr.deps)
    (cjars, rjars) = (jars.compiletime, jars.runtime)

    declared_dir = ctx.actions.declare_directory("%s" % (ctx.attr.name))

    inputs = [
        ctx.file.openapi_generator_cli,
        ctx.file.spec,
    ] + cjars.to_list() + rjars.to_list()

    # TODO: Convert to run
    ctx.actions.run_shell(
        inputs = inputs,
        command = "mkdir -p {gen_dir}".format(
            gen_dir = declared_dir.path,
        ) + " && " + _new_generator_command(ctx, declared_dir, rjars),
        outputs = [declared_dir],
        tools = ctx.files._jdk,
    )

    srcs = declared_dir.path

    return DefaultInfo(files = depset([
        declared_dir,
    ]))

def _collect_jars(targets):
    """Compute the runtime and compile-time dependencies from the given targets"""  # noqa
    compile_jars = depset()
    runtime_jars = depset()
    for target in targets:
        found = False
        if hasattr(target, "scala"):
            if hasattr(target.scala.outputs, "ijar"):
                compile_jars = depset(transitive = [compile_jars, [target.scala.outputs.ijar]])
            compile_jars = depset(transitive = [compile_jars, target.scala.transitive_compile_exports])
            runtime_jars = depset(transitive = [runtime_jars, target.scala.transitive_runtime_deps])
            runtime_jars = depset(transitive = [runtime_jars, target.scala.transitive_runtime_exports])
            found = True
        if hasattr(target, "java"):
            compile_jars = depset(transitive = [compile_jars, target.java.transitive_deps])
            runtime_jars = depset(transitive = [runtime_jars, target.java.transitive_runtime_deps])
            found = True
        if not found:
            runtime_jars = depset(transitive = [runtime_jars, target.files])
            compile_jars = depset(transitive = [compile_jars, target.files])

    return struct(compiletime = compile_jars, runtime = runtime_jars)

openapi_generator = rule(
    attrs = {
        # downstream dependencies
        "deps": attr.label_list(),
        # openapi spec file
        "spec": attr.label(
            mandatory = True,
            allow_single_file = [
                ".json",
                ".yaml",
            ],
        ),
        "generator": attr.string(mandatory = True),
        "api_package": attr.string(),
        "invoker_package": attr.string(),
        "model_package": attr.string(),
        "additional_properties": attr.string_dict(),
        "system_properties": attr.string_dict(),
        "type_mappings": attr.string_dict(),
        "_jdk": attr.label(
            default = Label("@bazel_tools//tools/jdk:current_java_runtime"),
            providers = [java_common.JavaRuntimeInfo],
        ),
        "openapi_generator_cli": attr.label(
            cfg = "host",
            default = Label("//external:io_bazel_rules_openapi_generator/dependency/openapi-generator-cli"),
            allow_single_file = True,
        ),
    },
    implementation = _impl,
)
