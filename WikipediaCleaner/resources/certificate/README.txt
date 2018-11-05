How to configure Code Signing for WPCleaner
===========================================

Make a copy of the previous WPCleaner.p12 file it it already exists.

== Obtaining a Code Signing Certificate ==
Note: this description is obsolete...

You can get a cheap Open Source Code Signing Certificate from certum.pl, for example.
Once you have received the mail confirming that the certificate has been created, follow the link to the certificate in your account.
Click on "Install online" in Chrome to install the certificate in Chrome.
Go to Chrome "Settings" ("Advanced Settings")
Click on "Manage certificates"
Select your certificate and click on "Export"
Choose to export the private key
Select PKCS#12, include all certificates in the path, and click on "Next"
Use the "WPCleaner KeyStore" password to protect the private key
Save the file as WPCleaner.p12

== Configure WPCleaner.p12 ==

Run the following command to retrieve the alias of the certificate:
keytool.exe -list -keystore WPCleaner.p12 -storepass <password> -storetype pkcs12 -v

Run the following command to Change the alias of the certificate to wpcleaner-sign:
keytool.exe -changealias -keystore WPCleaner.p12 -storepass <password> -storetype pkcs12 -destalias wpcleaner-sign -alias <alias>
