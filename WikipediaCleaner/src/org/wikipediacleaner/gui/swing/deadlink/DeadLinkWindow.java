/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.deadlink;

import java.util.List;

import javax.swing.JPanel;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.PanelWindow;
import org.wikipediacleaner.i18n.GT;


/**
 * Dead links. 
 */
public class DeadLinkWindow extends PanelWindow {

  /**
   * Create and display a DeadLinkWindow.
   * 
   * @param wiki Wiki.
   * @param errors List of dead link errors.
   * @param textPane Text pane.
   */
  public static void createDeadLinkWindow(
      EnumWikipedia wiki,
      final List<DeadLink> errors,
      final JTextComponent textPane) {
    JPanel panel = new DeadLinkPanel(wiki, errors, textPane);
    createPanelWindow("DeadLinkWindow", wiki, DeadLinkWindow.class, GT._T("Dead links"), panel);
  }
}
