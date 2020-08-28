#!/bin/bash

[ -n "$PMM_VERSION" ] || { echo "PMM_VERSION must be defined (x.y.z.nnnn)."; exit 1; }
[ -n "$PROJ_REL" ] || { echo "PROJ_REL must be defined (rc_commit)."; exit 1; }

rpmName="pmm-smppload-base"
rpmSpecFile=${HOME}/rpmbuild/SPECS/${rpmName}.spec

rm -f $rpmSpecFile

### base packages
cat >"${rpmSpecFile}" <<_SPEC_
%define  debug_package %{nil}
%define _unpackaged_files_terminate_build 0
%define buildroot_bak %{buildroot}

%define _source_filedigest_algorithm 0
%define _binary_filedigest_algorithm 0
%define _source_payload w6.bzdio
%define _binary_payload w6.bzdio

Name: ${rpmName}
Version: ${PMM_VERSION}
Release: ${PROJ_REL}
Summary: pmm smppload

Group: Applications/System
License: Proprietary
Source0: ${rpmName}-${PMM_VERSION}.tar

Requires: erlang > 17
BuildRequires: esl-erlang < 19
BuildRequires: libuuid-devel git net-snmp-utils tokyocabinet-devel ncurses-devel gcc-c++ zlib-devel bzip2-devel

%description

%pre
#id bms > /dev/null || exit 1

%prep
%setup -c -q

%build
make

%install
# for /usr/bin/ with system erlang
mkdir -p %{buildroot}/usr/bin
cp -a _build/default/bin/smppload %{buildroot}/usr/bin/
# for /opt/smppload with embed erlang
#mkdir -p %{buildroot}/opt/smppload
#cd _build/default/rel/smppload
#cp -a . %{buildroot}/opt/smppload/

%clean
rm -rf %{buildroot}

%files
# for /usr/bin/ with system erlang
/usr/bin/smppload
# for /opt/smppload with embed erlang
#/opt/smppload/

_SPEC_
###
