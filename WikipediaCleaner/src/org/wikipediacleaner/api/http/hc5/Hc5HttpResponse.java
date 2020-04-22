/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.http.hc5;

import java.io.InputStream;

/**
 * Bean for HTTP Response.
 */
public class Hc5HttpResponse {
  /** HTTP status of the response */
  final int status;

  /** Content of the response as an InputStream */
  final InputStream inputStream;

  public Hc5HttpResponse(int status) {
    this.status = status;
    this.inputStream = null;
  }

  public Hc5HttpResponse(int status, InputStream inputStream) {
    this.status = status;
    this.inputStream = inputStream;
  }
}
