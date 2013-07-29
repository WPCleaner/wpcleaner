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

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.Controller;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for analyzing a page.
 */
public class ActionFullAnalysis implements ActionListener {

  /**
   * Create a button for analyzing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-system-run.png", EnumImageSize.NORMAL,
          GT._("Full analysis"), false);
    } else {
      button = Utilities.createJButton(GT._("Full analysis"));
    }
    button.addActionListener(new ActionFullAnalysis(wiki, title));
    return button;
  }

  /**
   * Add a button for analyzing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title, boolean icon) {
    JButton button = createButton(wiki, title, icon);
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
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent,
      EnumWikipedia wiki, JList list,
      List<Page> knownPages, boolean icon) {
    JButton button;
    if (icon) {
      button = Utilities.createJButton(
          "gnome-system-run.png", EnumImageSize.NORMAL,
          GT._("Full analysis"), false);
    } else {
      button = Utilities.createJButton(GT._("Full analysis"));
    }
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
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      List<Page> knownPages, boolean icon) {
    JButton button = createButton(parent, wiki, list, knownPages, icon);
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
   * @param wiki Wiki.
   * @param title Page to be analyzed.
   */
  public ActionFullAnalysis(EnumWikipedia wiki, String title) {
    this.parent = null;
    this.wiki = wiki;
    this.title = title;
    this.list = null;
    this.knownPages = null;
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
    this.list = list;
    this.knownPages = knownPages;
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

    // Analyze a single page
    if ((list == null) && (title != null)) {
      Controller.runFullAnalysis(title, null, wiki);
    }

    // Analyze a list of selected pages
    if (list != null) {
      Controller.runFullAnalysis(parent, list.getSelectedValues(), knownPages, wiki);
    }
  }
}
