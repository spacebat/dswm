#!/bin/bash

IMG_PATH="/media/sdb1/dswm"
ARCH_="amd64"
#ADDITION_PACKAGES="sbcl,xserver-xorg,x11-xserver-utils,xnest,cl-clx-sbcl,cl-ppcre,grub,xinit,m4,autoconf,automake,autotools-dev,texi2html,texinfo,make,emacs23-nox,slime,w3m-el-snapshot,cl-mcclim,cl-mcclim-examples,cl-mcclim-doc,clisp-dev,clisp,git"
ADDITION_PACKAGES="sbcl,cl-clx-sbcl,cl-ppcre,m4,autoconf,automake,autotools-dev,texi2html,texinfo,make,emacs23-nox,slime,w3m-el-snapshot,cl-mcclim,cl-mcclim-examples,cl-mcclim-doc,clisp-dev,clisp,git"
DISTR=squeeze
GIT_SOURCE="https://github.com/dss-project/dswm.git"
GIT_BRANCH="trunk"
MK_FS="ext3"
HDD_SIZE="24800"
# git, autoconf
BOOT_ISO_URL="http://live.debian.net/cdimage/release/current/amd64/iso-hybrid/debian-live-6.0.1-amd64-standard.iso"

# Virtualbox config:
VBOX_MEM=1024
VBOX_CPU_NUM=1
VBOX_BOOT_ISO="$IMG_PATH"/boot.iso
VBOX_NET_INTERFACE=nat #none|null|nat|bridged|intnet|hostonly|vde

function prepare_image()
{
    mkdir -p "$IMG_PATH"/mount
    # Create image
    dd if=/dev/zero of="$IMG_PATH"/test.img bs=1M count=1 seek="$HDD_SIZE"
    mkfs -t "$MK_FS" -F "$IMG_PATH"/test.img
    # Mount
    mount -o loop "$IMG_PATH"test.img "$IMG_PATH"/mount
}

function _debootstrap_()
{
    if [ ! -d "$IMG_PATH"/mount ]; then
	mkdir -p "$IMG_PATH"/mount
    fi
    sudo debootstrap --arch=$ARCH_ --include=$ADDITION_PACKAGES $DISTR "$IMG_PATH"/mount
}

function _get_install_dswm_()
{
    mkdir -p "$IMG_PATH"/mount/home/dswm/
    git clone $GIT_SOURCE "$IMG_PATH"/mount/home/dswm/
    # if [ $1 = "install" ]; then
    # 	cd "$IMG_PATH"/mount/home/dswm/
    # 	autoconf
    # 	./configure
    # 	make distdir="$IMG_PATH"/mount install
    # fi
}

function env_modify()
{
    echo DISPLAY=:1 >> /etc/environment
    echo "LC_ALL=en_US.UTF-8"  >> /etc/environment

cat>"$IMG_PATH"/mount/usr/local/bin/start-dswm<<EOF
if [ $1 = "-bg" ]; then
DISPLAY=:1 dswm &
else
DISPLAY=:1 dswm
fi
EOF
chmod a+x "$IMG_PATH"/mount/usr/local/bin/start-dswm
echo "dswn-test" > /etc/hostname

cat>"$IMG_PATH"/mount/usr/local/bin/prepare<<EOF
while read i; do
export $i
done < /etc/environment
EOF
}

function _with_virtualbox_()
{
    if [ ! -z "$(mount | grep "$IMG_PATH")" ]; then
	umount "$IMG_PATH"/mount
    fi
    mkdir -p "$IMG_PATH"/vbox
    VBoxManage convertdd  "$IMG_PATH"/test.img "$IMG_PATH"/vbox/dswm_test.vdi
    VBoxManage createvm -name "DSWM_test" --basefolder "$IMG_PATH"/vbox -register
    VBoxManage storagectl "DSWM_test" --name "IDE Controller" --add ide
    VBoxManage storagectl "DSWM_test" --name "SATA" --add sata --bootable on
    VBoxManage storageattach "DSWM_test" --storagectl "SATA" --port 0 --device 0 --type hdd --medium "$IMG_PATH"/vbox/dswm_test.vdi
    VBoxManage storageattach "DSWM_test" --storagectl "SATA" --port 1 --device 0 --type dvddrive --medium "$VBOX_BOOT_ISO"
    VBoxManage modifyvm "DSWM_test" --memory "$VBOX_MEM" --cpus "$VBOX_CPU_NUM" --clipboard bidirectional --nic1 $VBOX_NET_INTERFACE --boot1 dvd
    VBoxManage startvm "DSWM_test"
    VBoxManage modifyvm "DSWM_test" --dvd none --boot1 hda
    echo "Now your virtualmashine is prepared for tests"
    # VBoxManage startvm "DSWM test"
}

if [ $UID = 0 ]; then

    if [ -z "$(ls "$IMG_PATH"/mount 2>/dev/null)" ] || [ $1 = "-f" ]; then
	#prepare_image
	_debootstrap_
	_get_install_dswm_
	env_modify
# _with_virtualbox_
    fi
if [ -z "$(mount | grep "$IMG_PATH"/mount/dev)" ]; then
    mount -o bind /dev "$IMG_PATH"/mount/dev
fi
if [ -z "$(mount | grep "$IMG_PATH"/mount/proc)" ]; then
    mount -t proc none "$IMG_PATH"/mount/proc
fi
if [ -z "$(mount | grep "$IMG_PATH"/mount/tmp)" ]; then
    mount -o bind /tmp "$IMG_PATH"/mount/tmp
fi
#sudo chroot "$IMG_PATH"/mount
#rm -rf /var/tmp/
else
    echo "You must be root!"
fi