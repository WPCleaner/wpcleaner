/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2016  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPClientConfig;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPFileFilter;
import org.apache.commons.net.ftp.FTPReply;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;

/**
 * WPCleaner dump downloader.
 */
public class DumpDownloader {

  private final static Log log = LogFactory.getLog(DumpDownloader.class);

  private final static int CODE_OK = 0;
  private final static int CODE_NOTHING_TO_DO = 1;
  private final static int CODE_ERROR = 2;

  /**
   * @param args
   */
  public static void main(String[] args) {
    log.info("Running dump downloader");

    // Various configuration
    Configuration config = Configuration.getConfiguration();
    EnumLanguage language = config.getLanguage();
    Locale.setDefault(language.getLocale());
    GT.setCurrentLanguage(config.getLanguage());

    // Analyze command line arguments
    int currentArg = 0;

    // Retrieve wiki code
    String wikiCode = null;
    if (args.length > currentArg) {
      wikiCode = args[currentArg];
      currentArg++;
    }

    // Retrieve HTTP server
    String server = null;
    if (args.length > currentArg) {
      server = args[currentArg];
      currentArg++;
    }

    // Retrieve base directory on server
    String baseDir = null;
    if (args.length > currentArg) {
      baseDir = args[currentArg];
      currentArg++;
    }

    // Retrieve local directory
    String localDir = null;
    if (args.length > currentArg) {
      localDir = args[currentArg];
      currentArg++;
    }

    // File to download
    String fileType = "pages-articles.xml.bz2";
    if (args.length > currentArg) {
      fileType = args[currentArg];
      currentArg++;
    }

    System.exit(downloadDump(wikiCode, server, baseDir, localDir, fileType));
  }

  /**
   * Download a dump file.
   * 
   * @param wikiCode Code for the wiki.
   * @param server FTP server.
   * @param baseDir Base directory in FTP server.
   * @param localDir Local directory for download.
   * @param fileType File type.
   * @return 0=OK, 1=Nothing to do, 2=Error.
   */
  private static int downloadDump(
      final String wikiCode,
      final String server,
      final String baseDir,
      final String localDir,
      final String fileType) {

    // Check parameters
    if ((wikiCode == null) ||
        (server == null) ||
        (baseDir == null) ||
        (localDir == null)) {
      System.exit(CODE_ERROR);
    }

    // Connect to FTP Server
    FTPClient ftp = new FTPClient();
    FTPClientConfig ftpConfig = new FTPClientConfig();
    ftp.configure(ftpConfig);
    int result = CODE_ERROR;
    try {

      // Connect to FTP server
      ftp.connect(server);
      log.info(ftp.getReplyString());
      int reply = ftp.getReplyCode();
      if (!FTPReply.isPositiveCompletion(reply)) {
        ftp.disconnect();
        log.warn("FTP server refused connection");
      }
      String username = "anonymous";
      String password = System.getProperty("user.name")+"@"+InetAddress.getLocalHost().getHostName();
      if (!ftp.login(username, password)) {
        reply = ftp.getReplyCode();
        log.error("FTP server refused login for anonymous: " + reply);
        return CODE_ERROR;
      }

      // List dumps hosted by the server
      if (!ftp.changeWorkingDirectory(baseDir)) {
        reply = ftp.getReplyCode();
        log.error("FTP server refused change working directory to " + baseDir + ": " + reply);
        return CODE_ERROR;
      }
      if (!ftp.changeWorkingDirectory(wikiCode)) {
        log.error("FTP server refused change working directory to " + wikiCode + ": " + reply);
        return CODE_ERROR;
      }
      List<String> dumps = new ArrayList<>();
      FTPFile[] directories = ftp.listDirectories();
      for (FTPFile directory : directories) {
        dumps.add(directory.getName());
      }
      Collections.sort(dumps);
      Collections.reverse(dumps);

      // Search in dumps if one has the requested file (recent first)
      for (String dump : dumps) {
        FTPFile[] files = ftp.listFiles(dump, new FTPFileFilter() {
          
          @Override
          public boolean accept(FTPFile file) {
            if (file == null) {
              return false;
            }
            if (!file.isFile() || file.isSymbolicLink()) {
              return false;
            }
            String name = file.getName();
            if (name.endsWith(fileType)) {
              return true;
            }
            return false;
          }
        });
        if (files.length > 0) {
          FTPFile ftpFile = files[0];
          ftp.setFileType(FTP.BINARY_FILE_TYPE);
          OutputStream output = null;

          // Check if exists
          String fileName = ftpFile.getName();
          File tmpFile = new File(localDir, fileName + "tmp");
          File destFile = new File(localDir, fileName);
          if (destFile.exists()) {
            log.info("Dump file already exists: " + destFile.getAbsolutePath());
            return CODE_NOTHING_TO_DO;
          }

          try {
            // Download
            log.info("Downloading : " + dump + "/" + fileName);
            output = new BufferedOutputStream(new FileOutputStream(tmpFile));
            if (ftp.retrieveFile(dump + "/" + fileName, output)) {
              output.close();
              log.info("Retrieved dump file " + fileName);
              if (destFile.exists()) {
                destFile.delete();
              }
              if (tmpFile.renameTo(destFile)) {
                return CODE_OK;
              }
              log.warn("Unable to rename file from " + tmpFile.getAbsolutePath() + " to " + destFile.getAbsolutePath());
            } else {
              reply = ftp.getReplyCode();
              log.warn("Unable to retrieve file " + fileName + ": " + reply);
            }
            result = CODE_ERROR;
          } catch (IOException e) {
            log.error("Error downloading dump file", e);
          } finally {
            if (output != null) {
              try {
                output.close();
              } catch (IOException e) {
                // Nothing to do
              }
              output = null;
            }
          }
        }
      }

      // Logout
      ftp.logout();

    } catch (Exception e) {
      log.error("Error downloading dump file", e);
    } finally {
      if (ftp.isConnected()) {
        try {
          ftp.disconnect();
        } catch (IOException e) {
          // Nothing to do
        }
      }
    }

    return result;
  }
}
