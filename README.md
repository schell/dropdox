Under Construction
==================
This software is not ready!

dropdox
-------
`dropdox` is a cheap replacement for DropBox that uses S3 as a Fuse filesystem. 
It allows you to drag and drop files right into S3 and is cost effective if you
have <= 333 GB of data stored in S3.

    Service  Price @ <= 2GB   Price @ <= 1TB   Price @ <= 49TB   Price @ <= 450 TB

    S3       $0.0300 / GB     $0.0300 / GB     $0.0295 / GB      $0.0290 
    Dropbox  free!            $9.99            $15.00 

Apart from price, it's also handy to have your files available out on the great
world wide web, instead of behind Dropbox's app. With S3's website mode you can
run your websites right from your desktop (even use git!), just like the good
old days.
