/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.http;

import java.util.Map;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.ResponseManager;


/**
 * Manage interactions with the tool server.
 */
public interface HttpServer {

  /**
   * Send a POST request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param properties Request properties.
   * @param manager Response manager.
   * @throws APIException Exception thrown by the API.
   */
  public void sendPost(
      String              path,
      Map<String, String> properties,
      ResponseManager     manager) throws APIException;

  /**
   * Send a GET request to the Tool Server.
   * 
   * @param path Path on the tool server.
   * @param manager Response manager.
   * @throws APIException Exception thrown by the API.
   */
  public void sendGet(
      String          path,
      ResponseManager manager) throws APIException;

  /**
   * @return Base URL.
   */
  public String getBaseUrl();
}
