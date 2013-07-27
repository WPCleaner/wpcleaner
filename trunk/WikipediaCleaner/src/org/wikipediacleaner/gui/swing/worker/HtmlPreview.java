/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.worker;


/**
 * An interface for HTML previewing.
 */
public interface HtmlPreview {

  /**
   * @param text HTML text.
   */
  public void setHtml(String text);
}
