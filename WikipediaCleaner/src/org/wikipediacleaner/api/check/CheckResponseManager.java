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
import java.nio.charset.StandardCharsets;
import java.util.Collection;

import org.wikipediacleaner.api.ResponseManager;
import org.wikipediacleaner.api.data.Page;

/**
 * Response manager for the list of detections in a page.
 */
class CheckResponseManager implements ResponseManager {

  /**
   * List of detections.
   */
  private final Collection<CheckWikiDetection> detections;

  private final Page page;

  /**
   * @param detections List of detections to be filled by the response manager.
   * @param page Page
   */
  public CheckResponseManager(
      Collection<CheckWikiDetection> detections,
      Page page) {
    this.detections = detections;
    this.page = page;
  }

  @Override
  public void manageResponse(InputStream stream) throws IOException {
    if (stream != null) {
      try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))) {
        String line;
        while ((line = reader.readLine()) != null) {
          CheckWikiDetection detection = CheckWikiDetection.analyzeLine(line, page);
          if (detection != null) {
            detections.add(detection);
          }
        }
      }
    }
  }
  
}