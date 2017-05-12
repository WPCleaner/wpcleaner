/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="https://wazeopedia.waze.com/wiki/USA/Main_Page">Wazeopedia USA</a>.
 */
public class WazeopediaUSA
  extends AbstractWikiSettings {

  /**
   * Default constructor.
   */
  public WazeopediaUSA() {
    super(
        "en", "Wazeopedia USA",
        new String[] {
            "wazeopedia.waze.com",
        },
        "/wiki/USA/api.php", "/wiki/USA/index.php",
        "wazeopediaUSA", null,
        ComponentOrientation.LEFT_TO_RIGHT);
  }
}
