before_commands:
    - "composer install --prefer-source"

tools:
    external_code_coverage:
        timeout: 600
    php_code_coverage:
        enabled: true
        test_command: ./vendor/bin/phpunit
    php_code_sniffer:
        enabled: true
        config:
            standard: PSR2
        filter:
            paths: ["src/*", "tests/*"]
    php_cpd:
        enabled: true
        excluded_dirs: ["build/*", "tests", "vendor"]
    php_cs_fixer:
        enabled: true
        config:
            level: all
        filter:
            paths: ["src/*", "tests/*"]
    php_loc:
        enabled: true
        excluded_dirs: ["build", "tests", "vendor"]
    php_mess_detector:
        enabled: true
        config:
            ruleset: phpmd.xml.dist
            design_rules: { eval_expression: false }
        filter:
            paths: ["src/*"]
    php_pdepend:
        enabled: true
        excluded_dirs: ["build", "tests", "vendor"]
    php_analyzer:
        enabled: true
        filter:
            paths: ["src/*", "tests/*"]
    php_hhvm:
        enabled: true
        filter:
            paths: ["src/*", "tests/*"]
    sensiolabs_security_checker: true
