sudo apt-get update
sudo apt-get install -y gcc g++ parallel libnlopt-dev git build-essential r-base r-base-dev r-base r-base-dev libatlas3-base llvm clang llvm gcc- libc++-dev libssl-dev libcurl4-openssl-dev libxml2-dev tmux libssl-dev libcurl4-openssl-dev gfortran libblas-dev liblapack-dev liblapack-dev liblapack3 glances

# config git
git config --global user.email "jackinovik@gmail.com"
git config --global user.name "Jacki Novik"
git config --global core.editor "vim"
ssh-keygen

# clone working repo
git clone git@github.com:jburos/stan-setup.git
cd stan-setup
git submodule update --init --recursive
Rscript setup.R

## (manually mounted working directory)
# inspect disk
sudo lsblk

# usually sdb

# format the disk
sudo mkfs.ext4 -m 0 -F -E lazy_itable_init=0,lazy_journal_init=0,discard /dev/sdb

sudo mkdir -p /mnt/disks/modelcache

sudo mount -o discard,defaults /dev/sdb /mnt/disks/modelcache

sudo chmod a+w /mnt/disks/modelcache

echo UUID=`sudo blkid -s UUID -o value /dev/sdb` /mnt/disks/modelcache ext4 discard,defaults,nofail 0 2 | sudo tee -a /etc/fstab
