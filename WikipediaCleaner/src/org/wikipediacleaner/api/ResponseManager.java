/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api;

import java.io.IOException;
import java.io.InputStream;


/**
 * Basic interface for managing response of an HTTP request.
 */
public interface ResponseManager {

  /**
   * Manage response of an HTTP request.
   * 
   * @param stream Response.
   * @throws IOException I/O exception.
   * @throws APIException Exception thrown by the API.
   */
  void manageResponse(InputStream stream) throws IOException, APIException;
}
