/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.algorithm;

import java.util.Map;

import org.wikipediacleaner.api.configuration.CWConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WikiConfiguration;

/**
 * Interface implemented by all detection of errors.
 */
public interface Algorithm {

  /* ========================================================================== */
  /* General information about the algorithm                                    */
  /* ========================================================================== */

  /**
   * @return Short description of the error.
   */
  String getShortDescription();

  /**
   * @return Short description of the error, stripped text.
   */
  String getShortDescriptionReplaced();

  /**
   * @return Long description of the error.
   */
  String getLongDescription();

  /**
   * @return Link to error description.
   */
  String getLink();

  /**
   * @return Flag indicating if this algorithm is available.
   */
  boolean isAvailable();

  /* ========================================================================== */
  /* Analysis                                                                   */
  /* ========================================================================== */

  /* ========================================================================== */
  /* Configuration                                                              */
  /* ========================================================================== */

  /**
   * @param wikiConfiguration Configuration for the wiki.
   * @param cwConfiguration Configuration for Check Wiki.
   * @param wpcConfiguration Configuration for WPCleaner.
   */
  void setConfiguration(
      WikiConfiguration wikiConfiguration,
      CWConfiguration cwConfiguration,
      WPCConfiguration wpcConfiguration);

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (key=name, value=description).
   */
  Map<String, AlgorithmParameter> getParameters();
}
