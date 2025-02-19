# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "ubuntu/bionic64"

  config.vm.provider "virtualbox" do |v|
    v.name = "openapi-generator"
    v.memory = 2048
    v.cpus = 2
  end

  config.vm.box_check_update = true

  #SSH
  config.ssh.forward_agent = true

  config.ssh.shell = "bash -c 'BASH_ENV=/etc/profile exec bash'"

  #Provision
  config.vm.provision "shell", inline: <<-SHELL
    sudo apt-get update
    sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
    sudo add-apt-repository  "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
          $(lsb_release -cs) stable"
    sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-cosmic main" > /etc/apt/sources.list.d/docker.list'
    sudo apt-get update
    sudo apt-get upgrade -y
    sudo apt-get install -y docker-ce
    sudo usermod -aG docker vagrant
  SHELL

end
