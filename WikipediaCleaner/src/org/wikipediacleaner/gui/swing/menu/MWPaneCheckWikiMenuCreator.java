/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.util.List;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;


/**
 * A helper class to manage contextual menu.
 */
public class MWPaneCheckWikiMenuCreator extends BasicMenuCreator {

  /**
   * @param popup Popup menu.
   * @param element Text element.
   * @param textPane Text pane.
   * @param info Information.
   */
  public void addInfo(
      JPopupMenu popup, Element element, JTextPane textPane,
      CheckErrorResult info) {
    if ((popup == null) || (element == null) || (textPane == null) || (info == null)) {
      return;
    }

    // Actions
    JMenuItem menuItem = null;
    List<Actionnable> possibleActions = info.getPossibleActions();
    if (possibleActions != null) {
      for (Actionnable possibleAction : possibleActions) {
        if (possibleAction.isCompositeAction()) {
          if (!possibleAction.getActions().isEmpty()) {
            JMenu subMenu = new JMenu(possibleAction.getName());
            for (Actionnable subAction : possibleAction.getActions()) {
              menuItem = new JMenuItem(subAction.getName());
              Action action = subAction.getAction(element, textPane);
              if (action != null) {
                menuItem.addActionListener(action);
              } else {
                menuItem.setEnabled(false);
              }
              subMenu.add(menuItem);
            }
            addSubmenu(popup, subMenu, 0, 0);
          }
        } else {
          menuItem = new JMenuItem(possibleAction.getName());
          Action action = possibleAction.getAction(element, textPane);
          if (action != null) {
            menuItem.addActionListener(action);
          } else {
            menuItem.setEnabled(false);
          }
          popup.add(menuItem);
        }
      }
    }

    // Error description
    addSeparator(popup);
    addDisabledText(popup, info.getErrorType());
  }

}
