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

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


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
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean redirect, boolean icon) {
    return createButton(wiki, title, redirect, null, icon);
  }

  /**
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param action Action to perform.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      EnumWikipedia wiki, String title,
      String action, boolean icon) {
    return createButton(wiki, title, false, action, icon);
  }

  /**
   * Create a button for viewing a page.
   * 
   * @param wiki Wiki.
   * @param title Page title.
   * @param redirect True if redirects should be followed.
   * @param action Action to perform.
   * @param icon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createButton(
      EnumWikipedia wiki, String title,
      boolean redirect, String action, boolean icon) {
    JButton button;
    if (icon) {
      if ((action != null) && (ACTION_HISTORY.equals(action))) {
        button = Utilities.createJButton(
            "gnome-emblem-documents.png", EnumImageSize.NORMAL,
            GT._("History (Alt + &H)"), false);
      } else {
        button = Utilities.createJButton(
            "gnome-emblem-web.png", EnumImageSize.NORMAL,
            GT._("External Viewer (Alt + &E)"), false);
      }
    } else {
      if ((action != null) && (ACTION_HISTORY.equals(action))) {
        button = Utilities.createJButton(GT._("&History"));
      } else {
        button = Utilities.createJButton(GT._("&External Viewer"));
      }
    }
    button.addActionListener(new ActionExternalViewer(wiki, title, redirect, action));
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
    this.redirect = redirect;
    this.action = action;
  }

  /**
   * View a page.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    if (title == null) {
      return;
    }
    if (action != null) {
      Utilities.browseURL(wiki, title, action);
    } else if (wiki != null) {
      Utilities.browseURL(wiki, title, redirect);
    } else {
      Utilities.browseURL(title);
    }
  }
}
