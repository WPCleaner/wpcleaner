/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.linter;

import java.util.List;

import javax.swing.JPanel;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.linter.LinterError;
import org.wikipediacleaner.gui.swing.basic.PanelWindow;
import org.wikipediacleaner.i18n.GT;


/**
 * Linter errors. 
 */
public class LinterErrorWindow extends PanelWindow {

  /**
   * Create and display an LinterErrorWindow.
   * 
   * @param wiki Wiki.
   * @param errors List of Linter errors.
   * @param textPane Text pane.
   */
  public static void createLinterErrorWindow(
      EnumWikipedia wiki,
      final List<LinterError> errors,
      final JTextComponent textPane) {
    JPanel panel = new LinterErrorPanel(wiki, errors, textPane);
    createPanelWindow("LinterWindow", wiki, LinterErrorWindow.class, GT._T("Linter errors"), panel);
  }
}
