/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.i18n;

import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.ResourceBundle;


/**
 * Base class for managing internationalization.
 */
public class Messages extends ResourceBundle {

  /**
   * Constructor.
   */
  public Messages() {
    super();
  }

  /**
   * @return Enumeration of keys in the bundle.
   * @see java.util.ResourceBundle#getKeys()
   */
  @Override
  public Enumeration<String> getKeys() {
    List<String> list = Collections.emptyList();
    return Collections.enumeration(list);
  }

  /**
   * Handle keys with an explanation inside (between square brackets)
   * 
   * @param key Key.
   * @return key without the explanation.
   * @see java.util.ResourceBundle#handleGetObject(java.lang.String)
   */
  @Override
  protected Object handleGetObject(String key) {
    if (key == null) {
      return null;
    }
    return key.replaceAll(" \\[X+\\]$", "");
  }
}
