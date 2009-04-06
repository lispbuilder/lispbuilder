#!/bin/sh

# Package LISPBUILDER-* for ASDF-INSTALL and Edi Weitz's Lisp Starter-Pack
# Sign, and the upload to balooga.com

# Setup account for automatic ssh login
# mkdir -p ~/.ssh
# chmod 700 ~/.ssh
# cd ~/.ssh
# ssh-keygen -t dsa
# scp -p id_dsa.pub user@yourserver.com:
# ssh remoteuser@remotehost
# cat id_dsa.pub >> ~/.ssh/authorized_keys

# Note that this also has to be done on the remote server to allow scp remote-to-remote copy

TARGET_DIR=~/lispbuilder/
SOURCE_DIR=~/lispbuilder-read-only/
SOURCE_BINARIES_DIR=~/lispbuilder/win32/

for LP in "$@"
do
    PACKAGE=lispbuilder-${LP}
    
    echo "Packaging ${LP} into ${PACKAGE}"
   
    VERSION=$(awk '/:version/ {print $2 }' ${SOURCE_DIR}${PACKAGE}/${PACKAGE}.asd | sed 's/"//g')
   
    echo "Removing ${TARGET_DIR}${PACKAGE}"
    echo "Removing ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz"
    echo "Removing ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc"
    rm -rf ${TARGET_DIR}${PACKAGE}
    rm -rf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz
    rm -rf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc
    
    echo "Exporting ${PACKAGE} from SVN to ${TARGET_DIR}${PACKAGE}"
    svn export ${SOURCE_DIR}${PACKAGE} ${TARGET_DIR}${PACKAGE}
    
    echo "Creating new package ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz"
    tar -C ${TARGET_DIR} -cvzf ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz ${PACKAGE} > nil
   
    echo "Signing ${PACKAGE}-${VERSION}.tgz"
    gpg -b -a ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz
    
    echo "Uploading: ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz >>"
    echo "           lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz"
    scp ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz
    echo "Uploading: ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc >>"
    echo "           lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz.asc"
    scp ${TARGET_DIR}${PACKAGE}-${VERSION}.tgz.asc lcluke@www.balooga.com:public_html/lispbuilder ${PACKAGE}-${VERSION}.tgz.asc

    # Upload to a different file name for ASDF-INSTALL.
    echo "Copying: lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz >>"
    echo "         lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}.tgz"
    scp lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}.tgz

    echo "Copying: lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz.asc >>"
    echo "         lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}.tgz.asc"
    scp lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}-${VERSION}.tgz.asc lcluke@www.balooga.com:public_html/lispbuilder/${PACKAGE}.tgz.asc
done

for LP in "$@"
do
    PACKAGE=lispbuilder-${LP}
    PACKAGE_BINARIES=${PACKAGE}-binaries
    PACKAGE_BINARIES_ASD=${PACKAGE_BINARIES}.asd

    echo "Exporting: ${SOURCE_DIR}${PACKAGE}/${PACKAGE_BINARIES_ASD} >>"
    echo "           ${SOURCE_BINARIES_DIR}${PACKAGE}/${PACKAGE_BINARIES_ASD}"
    svn export ${SOURCE_DIR}${PACKAGE}/${PACKAGE_BINARIES_ASD} ${SOURCE_BINARIES_DIR}${PACKAGE}/${PACKAGE_BINARIES_ASD}
          
    VERSION=$(awk '/:version / {print $2}' ${SOURCE_DIR}${PACKAGE}/${PACKAGE_BINARIES_ASD} | sed 's/"//g')
        
    echo "Removing ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz"
    rm -rf ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz
    
    echo "Creating new package ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz"
    tar -C ${SOURCE_BINARIES_DIR} -cvzf ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz ${PACKAGE} > nil
       
    echo "Uploading: ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz >>" 
    echo "           lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}-${VERSION}.tgz"
    scp ${TARGET_DIR}win32-${PACKAGE_BINARIES}-${VERSION}.tgz lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}-${VERSION}.tgz

    # Upload to a different file name for Edi's Starter Pack.
    echo "Copying: lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}-${VERSION}.tgz >>"
    echo "         lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}.tgz"
    scp lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}-${VERSION}.tgz lcluke@www.balooga.com:public_html/lispbuilder/win32-${PACKAGE_BINARIES}.tgz

done

echo "Uploading: ${TARGET_DIR}config.lisp >>"
echo "           lcluke@www.balooga.com:public_html/lispbuilder/config.lisp"
scp ${TARGET_DIR}config.lisp lcluke@www.balooga.com:public_html/lispbuilder/config.lisp
