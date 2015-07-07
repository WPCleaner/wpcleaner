/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.ResponseManager;

/**
 * Response manager for the list of detections in a page.
 */
class CheckResponseManager implements ResponseManager {

  /**
   * List of detections.
   */
  private final Collection<CheckWikiDetection> detections;

  /**
   * @param detections List of detections to be filled by the response manager.
   */
  public CheckResponseManager(
      Collection<CheckWikiDetection> detections) {
    this.detections = detections;
  }

  /**
   * @param stream
   * @throws IOException
   * @throws APIException
   * @see org.wikipediacleaner.api.ResponseManager#manageResponse(java.io.InputStream)
   */
  @Override
  public void manageResponse(InputStream stream) throws IOException,
      APIException {
    if (stream != null) {
      BufferedReader reader = null;
      try {
        reader = new BufferedReader(new InputStreamReader(stream, "UTF-8"));
        String line = null;
        while ((line = reader.readLine()) != null) {
          CheckWikiDetection detection = CheckWikiDetection.analyzeLine(line);
          if (detection != null) {
            detections.add(detection);
          }
        }
      } finally {
        if (reader != null) {
          try {
            reader.close();
          } catch (IOException e) {
            //
          }
        }
      }
    }
  }
  
}