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

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.InformationWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.CheckArticleTools;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Manage actions for checking a page.
 */
public class ActionCheckArticle implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createInternalButton(
      boolean showIcon, boolean showText, boolean useShortcut) {
    return Utilities.createJButton(
        showIcon ? "commons-text-x-generic-highlight-red-marker-round.png" : null,
        EnumImageSize.NORMAL,
        GT._("Check article"), showText,
        useShortcut ? ConfigurationValueShortcut.CHECK_ARTICLE : null);
  }

  /**
   * Create a button for checking a page.
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
    button.addActionListener(new ActionCheckArticle(wiki, title));
    return button;
  }

  /**
   * Add a button for checking a page.
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
   * Create a button for checking selected pages.
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
    JButton button = createInternalButton(showIcon, false, useShortcut);
    button.addActionListener(new ActionCheckArticle(parent, wiki, list));
    return button;
  }

  /**
   * Create a button for checking selected pages.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      boolean showIcon, boolean useShortcut) {
    JButton button = createButton(parent, wiki, list, showIcon, useShortcut);
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
   * Combo box containing the page name.
   */
  private final JComboBox combo;

  /**
   * @param wiki Wiki.
   * @param title Page to be analyzed.
   */
  public ActionCheckArticle(EnumWikipedia wiki, String title) {
    this.parent = null;
    this.wiki = wiki;
    this.title = title;
    this.list = null;
    this.text = null;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param list Selected pages should be analyzed.
   */
  public ActionCheckArticle(Component parent, EnumWikipedia wiki, JList list) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.list = list;
    this.text = null;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param text Text component containing the page name.
   */
  public ActionCheckArticle(Component parent, EnumWikipedia wiki, JTextComponent text) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.list = null;
    this.text = text;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param combo Combo box containing the page name.
   */
  public ActionCheckArticle(Component parent, EnumWikipedia wiki, JComboBox combo) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = null;
    this.list = null;
    this.text = null;
    this.combo = combo;
  }

  /**
   * Check a page.
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
      List<Page> pages = new ArrayList<Page>();
      for (Object element : list.getSelectedValues()) {
        if (element instanceof Page) {
          pages.add((Page) element);
        } else {
          pages.add(DataManager.getPage(wiki, element.toString(), null, null, null));
        }
      }
      checkArticles(pages, wiki);
      return;
    }

    // Analyze a single page
    String toAnalyze = null;
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
      toAnalyze = tmp.trim();
    }

    // Analyze a single page
    if ((toAnalyze == null) && (title != null)) {
      toAnalyze = title;
      return;
    }
    
    checkArticle(DataManager.getPage(wiki, toAnalyze, null, null, null), wiki);
  }

  /**
   * @param pages List of pages.
   * @param wiki Wiki.
   */
  private static void checkArticles(
      List<Page> pages, EnumWikipedia wiki) {
    CheckArticleTools tools = new CheckArticleTools(wiki);
    try {
      for (Page page : pages) {
        tools.checkArticle(page, null);
      }
    } catch (APIException e) {
      return;
    }
    String report = tools.getReport();
    InformationWindow.createInformationWindow(GT._("Analysis"), report, false, wiki);
  }

  /**
   * @param page Check an article.
   * @param wiki Wiki.
   */
  private static void checkArticle(
      Page page, EnumWikipedia wiki) {
    CheckArticleTools tools = new CheckArticleTools(wiki);
    try {
      tools.checkArticle(page, null);
    } catch (APIException e) {
      return;
    }
    String report = tools.getReport();
    InformationWindow.createInformationWindow(GT._("Analysis"), report, false, wiki);
  }
}
