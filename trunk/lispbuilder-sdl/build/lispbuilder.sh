#!/bin/sh

# Package LISPBUILDER-* for ASDF-INSTALL and Edi Weitz's Lisp Starter-Pack
# Sign, and the upload to balooga.com

#Setup account for automatic ssh login
#scp ~/.ssh/id_dsa.pub user@yourserver.com:
#cat id_dsa.pub >> .ssh/authorized_keys

TARGET_DIR=~/lispbuilder/
SOURCE_DIR=~/dev/lispbuilder/lispbuilder/
SOURCE_BINARIES_DIR=~/lispbuilder/win32/
USER_NAME=$1
REMOTE_SERVER=www.balooga.com
REMOTE_DIR=public_html/lispbuilder/
REMOTE_TARGET=${USER_NAME}@${REMOTE_SERVER}:${REMOTE_DIR}

# User name should be the first command line argument. Packages to process are the remainder.
shift

for LP in "$@"
do
    PACKAGE=lispbuilder-${LP}
    
    echo Packaging ${LP} into ${PACKAGE}
    
    VERSION=$(awk '/:version/ {print $2 }' ${SOURCE_DIR}${PACKAGE}/${PACKAGE}.asd | sed 's/"//g')
    
    echo Removing ${TARGET_DIR}${PACKAGE}
    echo Removing ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz
    echo Removing ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc
    rm -rf ${TARGET_DIR}${PACKAGE}
    rm -rf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz
    rm -rf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc
    
    echo "Exporting ${PACKAGE} from SVN to ${TARGET_DIR}${PACKAGE}"
    svn export ${SOURCE_DIR}${PACKAGE} ${TARGET_DIR}${PACKAGE}
    
    echo "Creating new package ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz"
    tar -C ${TARGET_DIR} -cvzf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz ${PACKAGE} > nil
   
    echo "Signing ${PACKAGE}-${VERSION}.tgz"
    gpg -b -a ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz
    
    echo "Uploading ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz"
    scp ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz ${REMOTE_TARGET}${PACKAGE}-${VERSION}.tgz
    echo "Uploading ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc"
    scp ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc ${REMOTE_TARGET}${PACKAGE}-${VERSION}.tgz.asc

    # Copy to different file name for ASDF-INSTALL.
    echo "Remote copying ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz to ${TARGET_DIR}${PACKAGE}.tgz for ASDF-INSTALL."
    scp ${REMOTE_TARGET}${TARGET_DIR}${PACKAGE}-${VERSION}.tgz ${REMOTE_TARGET}${PACKAGE}.tgz

    echo "Remote copying ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc to ${TARGET_DIR}${PACKAGE}.tgz.asc for ASDF-INSTALL."
    scp ${REMOTE_TARGET}${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc ${REMOTE_TARGET}${PACKAGE}.tgz.asc

    # Upload to a different file name for Edi's Starter Pack.
    #echo "Uploading ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz to ${TARGET_DIR}${PACKAGE}.tgz for Edi's Starter Pack."
    #scp ${REMOTE_TARGET}${TARGET_DIR}${PACKAGE}-${VERSION}.tgz ${REMOTE_TARGET}${PACKAGE}.tgz
done

for LP in "$@"
do
    PACKAGE=lispbuilder-${LP}
    PACKAGE_BINARIES=${PACKAGE}-binaries
          
    VERSION=$(awk '/:version/ {print $2 }' ${SOURCE_DIR}${PACKAGE}/${PACKAGE_BINARIES}.asd | sed 's/"//g')
    
    echo Removing ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz
    rm -rf ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz
    
    cp ${TARGET_DIR}${PACKAGE}/${PACKAGE_BINARIES}.asd ${SOURCE_BINARIES_DIR}${PACKAGE}/${PACKAGE_BINARIES}.asd

    echo "Creating new package ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz"
    tar -C ${SOURCE_BINARIES_DIR} -cvzf ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz ${PACKAGE} > nil
       
    echo "Uploading ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz"
    scp ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz ${REMOTE_TARGET}${PACKAGE_BINARIES}-${VERSION}.tgz

    # Upload to a different file name for Edi's Starter Pack.
    echo "Uploading ${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz to ${TARGET_DIR}${PACKAGE_BINARIES}.tgz."
    scp ${REMOTE_TARGET}${TARGET_DIR}${PACKAGE_BINARIES}-${VERSION}.tgz ${REMOTE_TARGET}${PACKAGE_BINARIES}.tgz
done

echo "Uploading config.lisp for Edi's Starter Pack."
scp ${TARGET_DIR}lispbuilder-sdl/starter-pack/config.lisp ${REMOTE_TARGET}config.lisp

echo "Uploading lispbuilder.sh."
scp $0 ${REMOTE_TARGET}$0
