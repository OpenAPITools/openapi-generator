'use strict';

const assert = require('assert');
const fs = require('fs');
const path = require('path');
const {execFileSync} = require('child_process');
const yaml = require('./js-yaml.js');

const root = path.resolve(__dirname, '..', '..');
const manifest = JSON.parse(fs.readFileSync(path.join(root, 'CI', 'change-scopes.json'), 'utf8'));
assert.strictEqual(manifest.version, 1, 'Unsupported CI scope manifest version');
assert(manifest.suites && typeof manifest.suites === 'object', 'Missing CI suites');

function matches(file, pattern) {
    pattern = pattern.replace(/\/+$/, '');
    if (!/[?*]/.test(pattern)) return file === pattern || file.startsWith(`${pattern}/`);
    const expression = pattern.split(/(\*\*|\*|\?)/).map(part => {
        if (part === '**') return '.*';
        if (part === '*') return '[^/]*';
        if (part === '?') return '[^/]';
        return part.replace(/[\\^$+.()|[\]{}]/g, '\\$&');
    }).join('');
    return new RegExp(`^${expression}$`).test(file) ||
        (pattern.endsWith('/**') && file === pattern.slice(0, -3));
}

function sampleRoots(suite) {
    return [...new Set([
        ...(suite.matrix?.sample || []),
        ...(suite.matrix?.include || []).filter(row => row.sample).map(row => row.sample)
    ])].map(dir => dir.replace(/\/+$/, ''));
}

const tracked = execFileSync('git', ['ls-files', '-z', '--', 'samples'], {
    cwd: root, encoding: 'utf8', maxBuffer: 32 * 1024 * 1024
}).split('\0').filter(Boolean);
const trackedSet = new Set(tracked);
const typescriptRoots = tracked.filter(file => file.endsWith('/tsconfig.json'))
    .map(file => path.posix.dirname(file))
    .filter(dir => trackedSet.has(`${dir}/.openapi-generator-ignore`) && trackedSet.has(`${dir}/package.json`));
const owned = Object.entries(manifest.suites).filter(([id]) => id.startsWith('samples-') ||
    id.startsWith('gradle-test.') || id.startsWith('circle.'));

function owners(dir) {
    return owned.filter(([, suite]) => {
        const roots = sampleRoots(suite);
        if (suite.discovery === 'typescript') roots.push(...typescriptRoots);
        return roots.some(candidate => matches(dir, candidate)) ||
            (suite.paths || []).some(pattern => matches(dir, pattern));
    }).map(([id]) => id);
}

// Config outputs and tracked generator markers reveal newly added samples even
// when no workflow path filter or matrix has been updated.
const candidates = new Set(tracked.filter(file => file.endsWith('/.openapi-generator-ignore'))
    .map(file => path.posix.dirname(file)));
const buildFiles = new Set(['pom.xml', 'package.json', 'pyproject.toml', 'Cargo.toml',
    'pubspec.yaml', 'go.mod', 'mix.exs', 'build.gradle', 'build.gradle.kts', 'Project.toml']);
for (const file of tracked) {
    const name = path.posix.basename(file);
    const directory = path.posix.dirname(file);
    if (directory !== 'samples' && (buildFiles.has(name) || name.endsWith('.csproj'))) candidates.add(directory);
}
for (const file of fs.readdirSync(path.join(root, 'bin', 'configs')).filter(file => file.endsWith('.yaml'))) {
    const text = fs.readFileSync(path.join(root, 'bin', 'configs', file), 'utf8');
    const output = text.match(/^outputDir:\s*(.+)$/m);
    if (output) {
        const dir = yaml.safeLoad(output[1]);
        if (typeof dir === 'string' && dir.startsWith('samples/')) candidates.add(dir.replace(/\/+$/, ''));
    }
}
const unowned = [...candidates].filter(dir => owners(dir).length === 0).sort();
if (process.argv.includes('--list-unowned')) {
    process.stdout.write(JSON.stringify(unowned, null, 2) + '\n');
    process.exit(0);
}

const exclusions = new Map((manifest.exclusions || []).map(entry => {
    assert(typeof entry.path === 'string' && entry.path.startsWith('samples/') &&
        !/[?*]/.test(entry.path), 'Exclusions must name exact sample roots');
    assert(typeof entry.reason === 'string' && entry.reason.trim(), `Missing exclusion reason: ${entry.path}`);
    return [entry.path, entry.reason];
}));
for (const dir of unowned) {
    assert(exclusions.has(dir), `Unowned sample ${dir}: register a CI suite or document an explicit exclusion`);
}

const workflowDir = path.join(root, '.github', 'workflows');
const workflows = new Map();
for (const file of fs.readdirSync(workflowDir).filter(file => /\.ya?ml$/.test(file))) {
    workflows.set(`.github/workflows/${file}`, yaml.safeLoad(fs.readFileSync(path.join(workflowDir, file), 'utf8')));
}
for (const [id, suite] of Object.entries(manifest.suites)) {
    for (const dir of sampleRoots(suite)) {
        assert(!dir.includes('..') && !path.posix.isAbsolute(dir), `Invalid sample root in ${id}: ${dir}`);
        assert(fs.statSync(path.join(root, ...dir.split('/'))).isDirectory(), `Missing sample target ${id}: ${dir}`);
        const packageFile = path.join(root, ...dir.split('/'), 'package.json');
        if (fs.existsSync(packageFile)) {
            const pkg = JSON.parse(fs.readFileSync(packageFile, 'utf8'));
            for (const reference of Object.values({...pkg.dependencies, ...pkg.devDependencies})) {
                if (typeof reference !== 'string' || !reference.startsWith('file:')) continue;
                const dependency = path.posix.normalize(path.posix.join(dir, reference.slice(5)));
                if (matches(dependency, dir)) continue;
                assert([...(suite.paths || []), ...(suite.shared_paths || [])].some(p => matches(dependency, p)),
                    `${id}: declare external local dependency ${dependency} as a shared input`);
            }
        }
    }
    if (!suite.workflow) continue;
    if (suite.workflow === '.circleci/config.yml') continue;
    const workflow = workflows.get(suite.workflow);
    assert(workflow, `Missing workflow for ${id}: ${suite.workflow}`);
    const calls = Object.values(workflow.jobs).flatMap(job => job.steps || [])
        .filter(step => step.uses === './.github/actions/compute-matrix' && step.with?.suite === id);
    assert.strictEqual(calls.length, 1, `Suite ${id} must have exactly one workflow selector`);
}
let sampleJobs = 0;
for (const [filename, workflow] of workflows) {
    if (!/\/samples-[^/]+\.ya?ml$/.test(filename) && !filename.endsWith('/gradle-test.yaml')) continue;
    assert(workflow.on.workflow_dispatch !== undefined, `${filename}: missing full-run dispatch`);
    assert(!workflow.on.pull_request?.paths, `${filename}: PR path filters can hide relevant changes`);
    assert(!workflow.on.push?.paths, `${filename}: safety pushes must run full scope`);
    assert.deepStrictEqual(workflow.on.push.branches, ['master', '[5-9]+.[0-9]+.x'],
        `${filename}: unexpected safety branches`);
    for (const [jobId, job] of Object.entries(workflow.jobs)) {
        if (jobId === 'setup') continue;
        sampleJobs++;
        const id = `${path.posix.basename(filename).replace(/\.ya?ml$/, '')}.${jobId}`;
        const suite = manifest.suites[id];
        assert(suite, `Missing scope for ${filename}/${jobId}`);
        assert.strictEqual(job.needs, 'setup', `${id}: selection must precede runner allocation`);
        assert(job.if?.includes("_changed == 'true'"), `${id}: missing explicit empty-selection guard`);
        if (suite.matrix) {
            assert.strictEqual(typeof job.strategy.matrix, 'string', `${id}: duplicate static matrix`);
            assert(job.strategy.matrix.includes('fromJSON(needs.setup.outputs.'), `${id}: disconnected matrix`);
        }
    }
}
console.log(`CI scope coverage: ${sampleJobs} sample jobs, ${Object.keys(manifest.suites).length} suites, ` +
    `${candidates.size} sample roots (${unowned.length} documented exclusions).`);
