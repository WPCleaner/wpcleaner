/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants.wiki;

import java.awt.ComponentOrientation;


/**
 * Configuration for <a href="https://uk.minecraft.wiki/">Ukrainian Minecraft</a>.
 */
public class UkMinecraft
  extends AbstractWikiSettings {

  /**
   * Default constructor.
   */
  public UkMinecraft() {
    super(
        "uk", "UK Minecraft",
        new String[] {
            "uk.minecraft.wiki",
        },
        "/api.php", "/index.php",
        "ukminecraft", null,
        ComponentOrientation.LEFT_TO_RIGHT);
  }
}
