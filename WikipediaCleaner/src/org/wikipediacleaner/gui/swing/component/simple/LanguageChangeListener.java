/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2019  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.component.simple;

import org.wikipediacleaner.api.constants.EnumLanguage;

/**
 * An interface for listening to changes in the language selection.
 */
public interface LanguageChangeListener {

  /**
   * Called when the language selection has changed.
   * 
   * @param language New language.
   */
  public void changeLanguage(EnumLanguage language);
}
