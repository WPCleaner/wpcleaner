/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Manage actions for adding a page to the Watch list.
 */
public class ActionWatchPage implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon, boolean useShortcut) {
    return ActionUtilities.createInternalButton(
        "gnome-logviewer-add.png", showIcon,
        GT._("Add to Watch list"), !showIcon,
        ConfigurationValueShortcut.ADD_TO_WATCH_LIST, useShortcut);
  }

  /**
   * Create a button for adding a page to the Watch list.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      Component parent,
      EnumWikipedia wiki, String title,
      boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(showIcon, useShortcut);
    button.addActionListener(new ActionWatchPage(parent, wiki, title));
    return button;
  }

  /**
   * Add a button for adding a page to the Watch list.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean showIcon, boolean useShortcut) {
    JButton button = createButton(parent, wiki, title, showIcon, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Create a button for adding selected pages to the Watch list.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list List.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      Component parent,
      EnumWikipedia wiki, JList list,
      boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(showIcon, useShortcut);
    button.addActionListener(new ActionWatchPage(parent, wiki, list));
    return button;
  }

  /**
   * Parent component.
   */
  private final Component parent;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * Page to be added to the watch list.
   */
  private final String title;

  /**
   * Selected pages in the JList should be added to the watch list.
   */
  private final JList list;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page to be added to the watch list.
   */
  private ActionWatchPage(Component parent, EnumWikipedia wiki, String title) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = title;
    this.list = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be added to the watch list.
   */
  private ActionWatchPage(Component parent, EnumWikipedia wiki, JList list) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.list = list;
  }

  /**
   * Add a page to the watch list.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (wiki == null)) {
      return;
    }

    // Add a single page
    if ((list == null) && (title != null)) {
      String message = GT._("Would you like to add this page on your local Watch list ?");
      int answer = Utilities.displayYesNoWarning(parent, message);
      if (answer == JOptionPane.YES_OPTION) {
        Configuration config = Configuration.getConfiguration();
        List<String> watch = config.getStringList(
            wiki, Configuration.ARRAY_WATCH_PAGES);
        if (!watch.contains(title)) {
          watch.add(title);
          Collections.sort(watch);
          config.setStringList(wiki, Configuration.ARRAY_WATCH_PAGES, watch);
        }
      }
    }

    // Add a list of selected pages
    if (list != null) {
      Object[] links = list.getSelectedValues();
      if ((links == null) || (links.length == 0)) {
        return;
      }
      String message = GT._("Would you like to add these pages on your local Watch list ?");
      int answer = Utilities.displayYesNoWarning(parent, message);
      if (answer == JOptionPane.YES_OPTION) {
        Configuration config = Configuration.getConfiguration();
        List<String> watch = config.getStringList(wiki, Configuration.ARRAY_WATCH_PAGES);
        boolean added = false;
        for (Object link : links) {
          if (!watch.contains(link.toString())) {
            added = true;
            watch.add(link.toString());
          }
        }
        if (added) {
          Collections.sort(watch);
          config.setStringList(wiki, Configuration.ARRAY_WATCH_PAGES, watch);
        }
      }
    }
  }
}
