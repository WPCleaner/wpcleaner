/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.algorithm;

import java.util.Map;

import org.wikipediacleaner.api.constants.CWConfiguration;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WikiConfiguration;

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
  public String getShortDescription();

  /**
   * @return Short description of the error, stripped text.
   */
  public String getShortDescriptionReplaced();

  /**
   * @return Long description of the error.
   */
  public String getLongDescription();

  /**
   * @return Link to error description.
   */
  public String getLink();

  /**
   * @return Flag indicating if this algorithm is available.
   */
  public boolean isAvailable();

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
  public void setConfiguration(
      WikiConfiguration wikiConfiguration,
      CWConfiguration cwConfiguration,
      WPCConfiguration wpcConfiguration);

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (key=name, value=description).
   */
  public Map<String, AlgorithmParameter> getParameters();
}
