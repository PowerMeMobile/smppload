#!/bin/bash

env | sort

# environment variables:
# CHECKOUT_TO - commit ID (with checkout)
# GIT_COMMIT - commit ID (without checkout)
#  CHECKOUT_TO or GIT_COMMIT is mandatory
# BUILD_NUMBER - build number in jenkins (mandatory but not fatal)
# PMM_VERSION - version in format x.y.z.nnnn (optional)

if [ -n "$CHECKOUT_TO" -a "no_hash" != "$CHECKOUT_TO" ]; then
	GIT_COMMIT=$CHECKOUT_TO
	git checkout -f $GIT_COMMIT
	if [ 0 -ne $? ]; then
		echo "commit number error!"
		exit 1
	fi
fi

TAG=$(git describe --always --tags --abbrev=10)
PROJ_VER=${TAG%%-*}
PROJ_REL=rc_${TAG##*-g}
RPM_NAME="pmm-smppload-base"

[ -n "$BUILD_NUMBER" ] || BUILD_NUMBER=0
[ -n "$PMM_VERSION" ] || PMM_VERSION="${PROJ_VER}.${BUILD_NUMBER}"

# PROJ_REL="${RPM_RELEASE_TYPE}_$(git log --raw --abbrev-commit --abbrev=10 | head -n1 | awk '{print $2}')"

#export GIT_BRANCH
#export RPM_RELEASE_TYPE
export PMM_VERSION
export PROJ_REL

function send_comment() {
	local retcode=$1
	~/bin/git_send_comment.sh $retcode "smppload" $BUILD_URL $PMM_VERSION $GIT_COMMIT
}

function build_rpm() {
set -x
	./builder_genspec.sh

	echo "######## build source rpm"
	rpmbuild -bs ${HOME}/rpmbuild/SPECS/${RPM_NAME}.spec
	rm -f ${HOME}/rpmbuild/SOURCES/${RPM_NAME}-${PMM_VERSION}-${PROJ_REL}.tar
	rm -f ${HOME}/rpmbuild/SPECS/${RPM_NAME}.spec

	echo "######## Start Mock!"
	local ret_code
	[ -d build ] || mkdir build
	/usr/bin/mock -r epel-6-pmm-x86_64 --resultdir=build --plugin-option=bind_mount:dirs="[(\"build\",\"/tmp/build\")]" --rebuild ${HOME}/rpmbuild/SRPMS/${RPM_NAME}-${PMM_VERSION}-${PROJ_REL}.src.rpm
#	/usr/bin/mock --offline -r epel-6-pmm-x86_64 --resultdir=build --plugin-option=bind_mount:dirs="[(\"build\",\"/tmp/build\")]" --rebuild ${HOME}/rpmbuild/SRPMS/${RPM_NAME}-${PMM_VERSION}-${PROJ_REL}.src.rpm
	ret_code=$?
	rm -f ${HOME}/rpmbuild/SRPMS/${RPM_NAME}-${PMM_VERSION}-${PROJ_REL}.src.rpm
set +x

	if [[ $ret_code -ne 0 ]]; then
		send_comment 1 $GIT_PROJECT $BUILD_URL $RPM_VERSION $GIT_COMMIT
		exit 1
	fi
}
###############

#git archive -o ${HOME}/rpmbuild/SOURCES/pmm-bms-webapp-${PMM_VERSION}-${PROJ_REL}.tar HEAD
DIR_NAME=${PWD##*/}
#pushd ..
#tar -cf ${HOME}/rpmbuild/SOURCES/${RPM_NAME}-${PMM_VERSION}.tar --owner=0 --group=0 "$DIR_NAME"
#popd
tar -cf ${HOME}/rpmbuild/SOURCES/${RPM_NAME}-${PMM_VERSION}.tar --owner=0 --group=0 .
build_rpm

echo "######## Done build!"

send_comment 0 $GIT_PROJECT $BUILD_URL $RPM_VERSION $GIT_COMMIT
