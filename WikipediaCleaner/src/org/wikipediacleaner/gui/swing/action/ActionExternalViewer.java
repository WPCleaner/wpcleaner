/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.action;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JToolBar;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.ConfigurationValueShortcut;


/**
 * Manage actions for viewing a page.
 */
public class ActionExternalViewer extends AbstractAction implements ActionListener {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = -2178967292945002584L;

  /**
   * Action for page history.
   */
  public static final String ACTION_HISTORY = "history";

  /**
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createInternalButton(
      String action,
      boolean showIcon, boolean useShortcut) {
    if ((action != null) && (ACTION_HISTORY.equals(action))) {
      return Utilities.createJButton(
          showIcon ? "gnome-emblem-documents.png" : null,
          EnumImageSize.NORMAL,
          GT._T("History"), !showIcon,
          useShortcut ? ConfigurationValueShortcut.HISTORY : null);
    }
    return Utilities.createJButton(
        showIcon ? "gnome-emblem-web.png" : null,
        EnumImageSize.NORMAL,
        GT._T("External Viewer"), !showIcon,
        useShortcut ? ConfigurationValueShortcut.EXTERNAL_VIEWER : null);
  }

  /**
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean redirect, boolean showIcon, boolean useShortcut) {
    return createButton(wiki, title, redirect, null, showIcon, useShortcut);
  }

  /**
   * Add a button for viewing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean redirect, boolean showIcon, boolean useShortcut) {
    return addButton(toolbar, wiki, title, redirect, null, showIcon, useShortcut);
  }

  /**
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      String action, boolean showIcon, boolean useShortcut) {
    return createButton(wiki, title, false, action, showIcon, useShortcut);
  }

  /**
   * Add a button for viewing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title,
      String action, boolean showIcon, boolean useShortcut) {
    return addButton(toolbar, wiki, title, false, action, showIcon, useShortcut);
  }

  /**
   * Create a button for viewing a list of selected pages.
   * 
   * @param wiki Wiki.
   * @param list List.
   * @param redirect True if redirects should be followed.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, JList list,
      boolean redirect, boolean showIcon, boolean useShortcut) {
    return createButton(wiki, list, redirect, null, showIcon, useShortcut);
  }

  /**
   * Add a button for viewing a list of selected pages.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param redirect True if redirects should be followed.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      boolean redirect, boolean showIcon, boolean useShortcut) {
    return addButton(toolbar, wiki, list, redirect, null, showIcon, useShortcut);
  }

  /**
   * Create a button for viewing a list of selected pages.
   * 
   * @param wiki Wiki.
   * @param list List.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, JList list,
      String action, boolean showIcon, boolean useShortcut) {
    return createButton(wiki, list, false, action, showIcon, useShortcut);
  }

  /**
   * Add a button for viewing a list of selected pages.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      String action, boolean showIcon, boolean useShortcut) {
    return addButton(toolbar, wiki, list, false, action, showIcon, useShortcut);
  }

  /**
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean redirect, String action,
      boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(action, showIcon, useShortcut);
    button.addActionListener(new ActionExternalViewer(wiki, title, redirect, action));
    return button;
  }

  /**
   * Add a button for viewing a page.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean redirect, String action,
      boolean showIcon, boolean useShortcut) {
    JButton button = createButton(wiki, title, redirect, action, showIcon, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Create a button for viewing a list of selected pages.
   * 
   * @param wiki Wiki.
   * @param list List.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton createButton(
      EnumWikipedia wiki, JList list,
      boolean redirect, String action,
      boolean showIcon, boolean useShortcut) {
    JButton button = createInternalButton(action, showIcon, useShortcut);
    button.addActionListener(new ActionExternalViewer(wiki, list, redirect, action));
    return button;
  }

  /**
   * Add a button for viewing a list of selected pages.
   * 
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param list List.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   * @param showIcon True if the button should use an icon.
   * @param useShortcut True if shortcut should be used.
   * @return Button.
   */
  private static JButton addButton(
      JToolBar toolbar,
      EnumWikipedia wiki, JList list,
      boolean redirect, String action,
      boolean showIcon, boolean useShortcut) {
    JButton button = createButton(wiki, list, redirect, action, showIcon, useShortcut);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  /**
   * Page which is to be displayed.
   */
  private final String title;

  /**
   * Selected pages in the JList should be displayed.
   */
  private final JList list;

  /**
   * True if redirects should be followed.
   */
  private final boolean redirect;

  /**
   * Action to perform.
   */
  private final String action;

  /**
   * @param url IRL to be displayed.
   */
  public ActionExternalViewer(String url) {
    this(null, url, false, null);
  }

  /**
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   */
  public ActionExternalViewer(EnumWikipedia wiki, String title) {
    this(wiki, title, false, null);
  }

  /**
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   * @param redirect True if redirects should be followed.
   */
  public ActionExternalViewer(
      EnumWikipedia wiki, String title,
      boolean redirect) {
    this(wiki, title, redirect, null);
  }

  /**
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   * @param action Action to perform.
   */
  public ActionExternalViewer(
      EnumWikipedia wiki, String title,
      String action) {
    this(wiki, title, false, action);
  }

  /**
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   */
  private ActionExternalViewer(
      EnumWikipedia wiki, String title,
      boolean redirect, String action) {
    this.wiki = wiki;
    this.title = title;
    this.list = null;
    this.redirect = redirect;
    this.action = action;
  }

  /**
   * @param wiki Wiki.
   * @param list Selected pages will be displayed.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   */
  private ActionExternalViewer(
      EnumWikipedia wiki, JList list,
      boolean redirect, String action) {
    this.wiki = wiki;
    this.title = null;
    this.list = list;
    this.redirect = redirect;
    this.action = action;
  }

  /**
   * View a page.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (list != null) {
      for (Object selection : list.getSelectedValuesList()) {
        if (selection instanceof Page) {
          viewPage(((Page) selection).getTitle());
        }
      }
    } else if (title != null) {
      viewPage(title);
    }
  }

  /**
   * View a page.
   * 
   * @param pageTitle Page title.
   */
  private void viewPage(String pageTitle) {
    if (action != null) {
      Utilities.browseURL(wiki, pageTitle, action);
    } else if (wiki != null) {
      Utilities.browseURL(wiki, pageTitle, redirect);
    } else {
      Utilities.browseURL(pageTitle);
    }
  }
}
