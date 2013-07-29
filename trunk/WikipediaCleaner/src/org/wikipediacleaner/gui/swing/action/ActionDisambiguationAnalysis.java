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

import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Manage actions for analyzing a page.
 */
public class ActionDisambiguationAnalysis implements ActionListener {

  /**
   * Create a button for analyzing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param icon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean icon, boolean showText) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "commons-disambig-colour.png", EnumImageSize.NORMAL,
          showText ? GT._("&Disambiguation") : GT._("Disambiguation (Alt + &D)"),
          showText);
    } else {
      button = Utilities.createJButton(GT._("Disambiguation"));
    }
    button.addActionListener(new ActionDisambiguationAnalysis(wiki, title));
    return button;
  }

  /**
   * Add a button for analyzing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param icon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean icon, boolean showText) {
    JButton button = createButton(wiki, title, icon, showText);
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
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent,
      EnumWikipedia wiki, JList list,
      boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "commons-disambig-colour.png", EnumImageSize.NORMAL,
          GT._("Disambiguation (Alt + &D)"), false);
    } else {
      button = Utilities.createJButton(GT._("Disambiguation"));
    }
    button.addActionListener(new ActionDisambiguationAnalysis(parent, wiki, list));
    return button;
  }

  /**
   * Create a button for analyzing selected pages.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      boolean icon) {
    JButton button = createButton(parent, wiki, list, icon);
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
  public ActionDisambiguationAnalysis(EnumWikipedia wiki, String title) {
    this.parent = null;
    this.wiki = wiki;
    this.title = title;
    this.list = null;
    this.text = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be analyzed.
   */
  public ActionDisambiguationAnalysis(Component parent, EnumWikipedia wiki, JList list) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.list = list;
    this.text = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param text Text component containing the page name.
   */
  public ActionDisambiguationAnalysis(Component parent, EnumWikipedia wiki, JTextComponent text) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
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
      Controller.runDisambiguationAnalysis(parent, list.getSelectedValues(), wiki);
      return;
    }

    // Analyze a single page
    if (text != null) {
      String tmp = text.getText();
      if ((tmp == null) || (tmp.trim().length() == 0)) {
        Utilities.displayWarning(
            parent,
            GT._("You must input a page name for running a disambiguation analysis"),
            text);
        return;
      }
      tmp = tmp.trim();
      Configuration config = Configuration.getConfiguration();
      config.setString(
          null, ConfigurationValueString.PAGE_NAME, tmp);
      config.save();
      Controller.runDisambiguationAnalysis(tmp, wiki);
      return;
    }

    // Analyze a single page
    if (title != null) {
      Controller.runDisambiguationAnalysis(title, wiki);
      return;
    }
  }
}
