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
import java.util.List;

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.ConfigurationValueString;
import org.wikipediacleaner.utils.ConfigurationValueShortcut.ShortcutProperties;


/**
 * Manage actions for analyzing a page.
 */
public class ActionFullAnalysis implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon, boolean showText, boolean useShortcut) {
    JButton button = null;
    ShortcutProperties shortcut = null;
    if (useShortcut) {
      Configuration config = Configuration.getConfiguration();
      shortcut = config.getShortcut(ConfigurationValueShortcut.FULL_ANALYSIS);
    }
    if (showIcon) {
      button = Utilities.createJButton(
          "gnome-system-run.png", EnumImageSize.NORMAL,
          GT._("Full analysis"), showText, shortcut);
    } else {
      button = Utilities.createJButton(GT._("Full analysis"), shortcut);
    }
    return button;
  }

  /**
   * Create a button for analyzing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean showIcon, boolean showText, boolean useShortcut) {
    JButton button = createInternalButton(showIcon, showText, useShortcut);
    button.addActionListener(new ActionFullAnalysis(wiki, title));
    return button;
  }

  /**
   * Add a button for analyzing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean showIcon, boolean showText, boolean useShortcut) {
    JButton button = createButton(wiki, title, showIcon, showText, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Create a button for analyzing selected pages.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list List.
   * @param knownPages List of knownPages.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      Component parent,
      EnumWikipedia wiki, JList list,
      List<Page> knownPages, boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(showIcon, false, useShortcut);
    button.addActionListener(new ActionFullAnalysis(parent, wiki, list, knownPages));
    return button;
  }

  /**
   * Create a button for analyzing selected pages.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param knownPages List of knownPages.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      List<Page> knownPages, boolean showIcon, boolean useShortcut) {
    JButton button = createButton(parent, wiki, list, knownPages, showIcon, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
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
   * Page to be analyzed.
   */
  private final String title;

  /**
   * List of known pages.
   */
  private final List<Page> knownPages;

  /**
   * Selected pages in the JList should be analyzed.
   */
  private final JList list;

  /**
   * Text component containing the page name.
   */
  private final JTextComponent text;

  /**
   * @param wiki Wiki.
   * @param title Page to be analyzed.
   */
  public ActionFullAnalysis(EnumWikipedia wiki, String title) {
    this.parent = null;
    this.wiki = wiki;
    this.title = title;
    this.knownPages = null;
    this.list = null;
    this.text = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be analyzed.
   * @param knownPages List of known pages.
   */
  public ActionFullAnalysis(Component parent, EnumWikipedia wiki, JList list, List<Page> knownPages) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.knownPages = knownPages;
    this.list = list;
    this.text = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param text Text component containing the page name.
   */
  public ActionFullAnalysis(Component parent, EnumWikipedia wiki, JTextComponent text) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.knownPages = null;
    this.list = null;
    this.text = text;
  }

  /**
   * Analyze a page.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (wiki == null)) {
      return;
    }

    // Analyze a list of selected pages
    if (list != null) {
      Controller.runFullAnalysis(parent, list.getSelectedValues(), knownPages, wiki);
      return;
    }

    // Analyze a single page
    if (text != null) {
      String tmp = text.getText();
      if ((tmp == null) || (tmp.trim().length() == 0)) {
        Utilities.displayWarning(
            parent,
            GT._("You must input a page name for running a full analysis"),
            text);
        return;
      }
      tmp = tmp.trim();
      Configuration config = Configuration.getConfiguration();
      config.setString(
          null, ConfigurationValueString.PAGE_NAME, tmp);
      config.save();
      Controller.runFullAnalysis(tmp, null, wiki);
      return;
    }

    // Analyze a single page
    if (title != null) {
      Controller.runFullAnalysis(title, null, wiki);
      return;
    }
  }
}
