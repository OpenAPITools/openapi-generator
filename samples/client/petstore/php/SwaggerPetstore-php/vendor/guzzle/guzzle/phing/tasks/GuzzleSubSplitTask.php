<?php
/**
 * Phing wrapper around git subsplit.
 *
 * @see https://github.com/dflydev/git-subsplit
 * @copyright 2012 Clay Loveless <clay@php.net>
 * @license   http://claylo.mit-license.org/2012/ MIT License
 */

require_once 'phing/tasks/ext/git/GitBaseTask.php';

// base - base of tree to split out
// subIndicatorFile - composer.json, package.xml?
class GuzzleSubSplitTask extends GitBaseTask
{
    /**
     * What git repository to pull from and publish to
     */
    protected $remote = null;

    /**
     * Publish for comma-separated heads instead of all heads
     */
    protected $heads = null;

    /**
     * Publish for comma-separated tags instead of all tags
     */
    protected $tags = null;

    /**
     * Base of the tree RELATIVE TO .subsplit working dir
     */
    protected $base = null;

    /**
     * The presence of this file will indicate that the directory it resides
     * in is at the top level of a split.
     */
    protected $subIndicatorFile = 'composer.json';

    /**
     * Do everything except actually send the update.
     */
    protected $dryRun = null;

    /**
     * Do not sync any heads.
     */
    protected $noHeads = false;

    /**
     * Do not sync any tags.
     */
    protected $noTags = false;

    /**
     * The splits we found in the heads
     */
    protected $splits;

    public function setRemote($str)
    {
        $this->remote = $str;
    }

    public function getRemote()
    {
        return $this->remote;
    }

    public function setHeads($str)
    {
        $this->heads = explode(',', $str);
    }

    public function getHeads()
    {
        return $this->heads;
    }

    public function setTags($str)
    {
        $this->tags = explode(',', $str);
    }

    public function getTags()
    {
        return $this->tags;
    }

    public function setBase($str)
    {
        $this->base = $str;
    }

    public function getBase()
    {
        return $this->base;
    }

    public function setSubIndicatorFile($str)
    {
        $this->subIndicatorFile = $str;
    }

    public function getSubIndicatorFile()
    {
        return $this->subIndicatorFile;
    }

    public function setDryRun($bool)
    {
        $this->dryRun = (bool) $bool;
    }

    public function getDryRun()
    {
        return $this->dryRun;
    }

    public function setNoHeads($bool)
    {
        $this->noHeads = (bool) $bool;
    }

    public function getNoHeads()
    {
        return $this->noHeads;
    }

    public function setNoTags($bool)
    {
        $this->noTags = (bool) $bool;
    }

    public function getNoTags()
    {
        return $this->noTags;
    }

    /**
     * GitClient from VersionControl_Git
     */
    protected $client   = null;

    /**
     * The main entry point
     */
    public function main()
    {
        $repo = $this->getRepository();
        if (empty($repo)) {
            throw new BuildException('"repository" is a required parameter');
        }

        $remote = $this->getRemote();
        if (empty($remote)) {
            throw new BuildException('"remote" is a required parameter');
        }

        chdir($repo);
        $this->client = $this->getGitClient(false, $repo);

        // initalized yet?
        if (!is_dir('.subsplit')) {
            $this->subsplitInit();
        } else {
            // update
            $this->subsplitUpdate();
        }

        // find all splits based on heads requested
        $this->findSplits();

        // check that GitHub has the repos
        $this->verifyRepos();

        // execute the subsplits
        $this->publish();
    }

    public function publish()
    {
        $this->log('DRY RUN ONLY FOR NOW');
        $base = $this->getBase();
        $base = rtrim($base, '/') . '/';
        $org = $this->getOwningTarget()->getProject()->getProperty('github.org');

        $splits = array();

        $heads = $this->getHeads();
        foreach ($heads as $head) {
            foreach ($this->splits[$head] as $component => $meta) {
                $splits[] = $base . $component . ':git@github.com:'. $org.'/'.$meta['repo'];
            }

            $cmd = 'git subsplit publish ';
            $cmd .= escapeshellarg(implode(' ', $splits));

            if ($this->getNoHeads()) {
                $cmd .= ' --no-heads';
            } else {
                $cmd .= ' --heads='.$head;
            }

            if ($this->getNoTags()) {
                $cmd .= ' --no-tags';
            } else {
                if ($this->getTags()) {
                    $cmd .= ' --tags=' . escapeshellarg(implode(' ', $this->getTags()));
                }
            }

            passthru($cmd);
        }
    }

    /**
     * Runs `git subsplit update`
     */
    public function subsplitUpdate()
    {
        $repo = $this->getRepository();
        $this->log('git-subsplit update...');
        $cmd = $this->client->getCommand('subsplit');
        $cmd->addArgument('update');
        try {
            $cmd->execute();
        } catch (Exception $e) {
            throw new BuildException('git subsplit update failed'. $e);
        }
        chdir($repo . '/.subsplit');
        passthru('php ../composer.phar update --dev');
        chdir($repo);
    }

    /**
     * Runs `git subsplit init` based on the remote repository.
     */
    public function subsplitInit()
    {
        $remote = $this->getRemote();
        $cmd = $this->client->getCommand('subsplit');
        $this->log('running git-subsplit init ' . $remote);

        $cmd->setArguments(array(
            'init',
            $remote
        ));

        try {
            $output = $cmd->execute();
        } catch (Exception $e) {
            throw new BuildException('git subsplit init failed'. $e);
        }
        $this->log(trim($output), Project::MSG_INFO);
        $repo = $this->getRepository();
        chdir($repo . '/.subsplit');
        passthru('php ../composer.phar install --dev');
        chdir($repo);
    }

    /**
     * Find the composer.json files using Phing's directory scanner
     *
     * @return array
     */
    protected function findSplits()
    {
        $this->log("checking heads for subsplits");
        $repo = $this->getRepository();
        $base = $this->getBase();

        $splits = array();
        $heads = $this->getHeads();

        if (!empty($base)) {
            $base = '/' . ltrim($base, '/');
        } else {
            $base = '/';
        }

        chdir($repo . '/.subsplit');
        foreach ($heads as $head) {
            $splits[$head] = array();

            // check each head requested *BEFORE* the actual subtree split command gets it
            passthru("git checkout '$head'");
            $ds = new DirectoryScanner();
            $ds->setBasedir($repo . '/.subsplit' . $base);
            $ds->setIncludes(array('**/'.$this->subIndicatorFile));
            $ds->scan();
            $files = $ds->getIncludedFiles();

            // Process the files we found
            foreach ($files as $file) {
                $pkg = file_get_contents($repo . '/.subsplit' . $base .'/'. $file);
                $pkg_json = json_decode($pkg, true);
                $name = $pkg_json['name'];
                $component = str_replace('/composer.json', '', $file);
                // keep this for split cmd
                $tmpreponame = explode('/', $name);
                $reponame = $tmpreponame[1];
                $splits[$head][$component]['repo'] = $reponame;
                $nscomponent = str_replace('/', '\\', $component);
                $splits[$head][$component]['desc'] = "[READ ONLY] Subtree split of $nscomponent: " . $pkg_json['description'];
            }
        }

        // go back to how we found it
        passthru("git checkout master");
        chdir($repo);
        $this->splits = $splits;
    }

    /**
     * Based on list of repositories we determined we *should* have, talk
     * to GitHub and make sure they're all there.
     *
     */
    protected function verifyRepos()
    {
        $this->log('verifying GitHub target repos');
        $github_org = $this->getOwningTarget()->getProject()->getProperty('github.org');
        $github_creds = $this->getOwningTarget()->getProject()->getProperty('github.basicauth');

        if ($github_creds == 'username:password') {
            $this->log('Skipping GitHub repo checks. Update github.basicauth in build.properties to verify repos.', 1);
            return;
        }

        $ch = curl_init('https://api.github.com/orgs/'.$github_org.'/repos?type=all');
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($ch, CURLOPT_USERPWD, $github_creds);
        // change this when we know we can use our bundled CA bundle!
        curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
        $result = curl_exec($ch);
        curl_close($ch);
        $repos = json_decode($result, true);
        $existing_repos = array();

        // parse out the repos we found on GitHub
        foreach ($repos as $repo) {
            $tmpreponame = explode('/', $repo['full_name']);
            $reponame = $tmpreponame[1];
            $existing_repos[$reponame] = $repo['description'];
        }

        $heads = $this->getHeads();
        foreach ($heads as $head) {
            foreach ($this->splits[$head] as $component => $meta) {

                $reponame = $meta['repo'];

                if (!isset($existing_repos[$reponame])) {
                    $this->log("Creating missing repo $reponame");
                    $payload = array(
                        'name' => $reponame,
                        'description' => $meta['desc'],
                        'homepage' => 'http://www.guzzlephp.org/',
                        'private' => true,
                        'has_issues' => false,
                        'has_wiki' => false,
                        'has_downloads' => true,
                        'auto_init' => false
                    );
                    $ch = curl_init('https://api.github.com/orgs/'.$github_org.'/repos');
                    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
                    curl_setopt($ch, CURLOPT_USERPWD, $github_creds);
                    curl_setopt($ch, CURLOPT_HTTPHEADER, array('Content-Type: application/json'));
                    curl_setopt($ch, CURLOPT_POST, 1);
                    curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($payload));
                    // change this when we know we can use our bundled CA bundle!
                    curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, false);
                    $result = curl_exec($ch);
                    echo "Response code: ".curl_getinfo($ch, CURLINFO_HTTP_CODE)."\n";
                    curl_close($ch);
                } else {
                    $this->log("Repo $reponame exists", 2);
                }
            }
        }
    }
}
