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
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.check.CheckErrorPage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;
import org.wikipediacleaner.utils.ConfigurationValueString;


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
    return Utilities.createJButton(
        showIcon ? "gnome-system-run.png" : null,
        EnumImageSize.NORMAL,
        GT._("Full analysis"), showText,
        useShortcut ? ConfigurationValueShortcut.FULL_ANALYSIS : null);
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
      EnumWikipedia wiki, JList<? extends Object> list,
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
      EnumWikipedia wiki, JList<? extends Object> list,
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
  private final JList<? extends Object> list;

  /**
   * Text component containing the page name.
   */
  private final JTextComponent text;

  /**
   * Combo box containing the page name.
   */
  private final JComboBox combo;

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
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be analyzed.
   * @param knownPages List of known pages.
   */
  public ActionFullAnalysis(Component parent, EnumWikipedia wiki, JList<? extends Object> list, List<Page> knownPages) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.knownPages = knownPages;
    this.list = list;
    this.text = null;
    this.combo = null;
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
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param combo Combo box containing the page name.
   */
  public ActionFullAnalysis(Component parent, EnumWikipedia wiki, JComboBox combo) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.knownPages = null;
    this.list = null;
    this.text = null;
    this.combo = combo;
  }

  /**
   * Analyze a page.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (wiki == null)) {
      return;
    }

    // Analyze a list of selected pages
    if (list != null) {
      List<? extends Object> selection = list.getSelectedValuesList();
      List<Page> pages = new ArrayList<Page>();
      if (selection != null) {
        for (Object object : selection) {
          if (object instanceof Page) {
            pages.add((Page) object);
          } else if (object instanceof CheckErrorPage) {
            pages.add(((CheckErrorPage) object).getPage());
          }
        }
      }
      Controller.runFullAnalysis(parent, pages, knownPages, wiki);
      return;
    }

    // Analyze a single page
    if ((text != null) || (combo != null)) {
      String tmp = null;
      if (text != null) {
        tmp = text.getText();
      } else {
        Object select = combo.getSelectedItem();
        if (select != null) {
          tmp = select.toString();
        }
      }
      if ((tmp == null) || (tmp.trim().length() == 0)) {
        Utilities.displayWarning(
            parent,
            GT._("You must input a page name for running a full analysis"),
            (text != null) ?  text : combo);
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
