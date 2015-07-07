/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2014  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.ResponseManager;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;

/**
 * Response manager for the list of pages.
 */
class PagesResponseManager implements ResponseManager {

  /**
   * True if classic interface should be used.
   */
  private final boolean classic;

  /**
   * Algorithm.
   */
  private final Integer errorNumber;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * List of errors.
   */
  private final List<CheckError> errors;

  /**
   * @param classic True if classic interface should be used.
   * @param algorithm Algorithm.
   * @param wiki Wiki.
   * @param errors List of errors to be filled by the response manager.
   */
  public PagesResponseManager(
      boolean classic,
      CheckErrorAlgorithm algorithm,
      EnumWikipedia wiki,
      List<CheckError> errors) {
    this.classic = classic;
    Integer tmp = null;
    try {
      tmp = Integer.valueOf(algorithm.getErrorNumberString());
    } catch (NumberFormatException e) {
      //
    }
    this.errorNumber = tmp;
    this.wiki = wiki;
    this.errors = errors;
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
    if (classic) {
      CheckError.addCheckErrorClassic(
          errors, wiki,
          errorNumber, stream);
    } else {
      CheckError.addCheckErrorBots(
          errors, wiki,
          errorNumber, stream);
    }
  }
  
}