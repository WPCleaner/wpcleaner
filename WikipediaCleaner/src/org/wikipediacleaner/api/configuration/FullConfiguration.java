/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.configuration;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.dataaccess.WikiProvider;

/**
 * Group all parts of the configuration.
 */
public class FullConfiguration implements WikiProvider {

  /** Wiki */
  private final EnumWikipedia wiki;

  /** Configuration for the wiki */
  private final WikiConfiguration wikiConfiguration;

  /** Configuration for WPC */
  private final WPCConfiguration wpcConfiguration;

  /** Configuration for Check Wiki */
  private final CWConfiguration cwConfiguration;

  /**
   * Constructor.
   * 
   * @param wiki Wiki.
   */
  public FullConfiguration(EnumWikipedia wiki) {
    this.wiki = wiki;
    this.wikiConfiguration = new WikiConfiguration();
    this.wpcConfiguration = new WPCConfiguration(wiki);
    this.cwConfiguration = new CWConfiguration(wiki.getSettings().getCodeCheckWiki(), wiki);
  }

  /**
   * @return Wiki.
   */
  @Override
  public EnumWikipedia getWiki() {
    return wiki;
  }

  /**
   * @return Configuration for the wiki.
   */
  public WikiConfiguration wiki() {
    return wikiConfiguration;
  }

  /**
   * @return Configuration for WPC.
   */
  public WPCConfiguration wpc() {
    return wpcConfiguration;
  }

  /**
   * @return Configuration for Check Wiki.
   */
  public CWConfiguration cw() {
    return cwConfiguration;
  }
}
