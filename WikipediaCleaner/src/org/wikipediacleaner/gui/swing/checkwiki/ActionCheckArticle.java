/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.checkwiki;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JToolBar;

import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckWiki;
import org.wikipediacleaner.api.check.CheckWikiDetection;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for checking an article with Check Wiki.
 */
public class ActionCheckArticle extends AbstractAction implements ActionListener {

  /**
   * Serialization.
   */
  private static final long serialVersionUID = 5745075609820714955L;

  /**
   * Action for page history.
   */
  public static final String ACTION_HISTORY = "history";

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    return Utilities.createJButton(
        showIcon ? "commons-nuvola-web-broom.png" : null,
        EnumImageSize.NORMAL,
        GT._("Check article with CheckWiki"), !showIcon,
        null);
  }

  /**
   * Create a button for checking an article.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, EnumWikipedia wiki,
      String title, boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionCheckArticle(parent, wiki, title));
    return button;
  }

  /**
   * Add a button for checking an article.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki, String title,
      boolean showIcon) {
    JButton button = createButton(parent, wiki, title, showIcon);
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
   * Page which is to be displayed.
   */
  private final String title;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   */
  private ActionCheckArticle(
      Component parent, EnumWikipedia wiki, String title) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = title;
  }

  /**
   * Check an article.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed(ActionEvent e) {
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    Page page = DataManager.getPage(wiki, title, null, null, null);
    Collection<CheckWikiDetection> detections = checkWiki.check(page);
    if (detections == null) {
      Utilities.displayWarning(
          parent,
          GT._("Unable to retrieve analysis from CheckWiki."));
      return;
    }
    if (detections.isEmpty()) {
      Utilities.displayInformationMessage(
          parent,
          GT._("No errors are currently detected by CheckWiki."));
      return;
    }
    StringBuilder message = new StringBuilder();
    message.append(GT._("The following errors are currently detected by CheckWiki:"));
    for (CheckWikiDetection detection : detections) {
      message.append("\n  ");
      message.append(detection.getLine());
    }
    Utilities.displayInformationMessage(parent, message.toString());
  }
}
