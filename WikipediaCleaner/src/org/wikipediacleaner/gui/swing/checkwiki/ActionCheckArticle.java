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
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

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

  /** Serialization. */
  private static final long serialVersionUID = 5745075609820714955L;

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    return Utilities.createJButton(
        showIcon ? "commons-nuvola-web-broom.png" : null,
        EnumImageSize.NORMAL,
        GT._T("Check article with CheckWiki"), !showIcon,
        null);
  }

  /**
   * Create a button for checking an article.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionCheckArticle(parent, wiki, title, textPane));
    return button;
  }

  /**
   * Add a button for checking an article.
   * 
   * @param parent Parent component.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, JToolBar toolbar,
      EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createButton(parent, wiki, title, textPane, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Parent component. */
  private final Component parent;

  /** Wiki. */
  private final EnumWikipedia wiki;

  /** Page which is to be displayed. */
  private final String title;

  /** Text pane where the text is. */
  private final JTextComponent textPane;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page which is to be displayed.
   * @param textPane Text pane where the text is.
   */
  private ActionCheckArticle(
      Component parent, EnumWikipedia wiki,
      String title, JTextComponent textPane) {
    this.parent = parent;
    this.wiki = wiki;
    this.title = title;
    this.textPane = textPane;
  }

  /**
   * Check an article.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    CheckWiki checkWiki = APIFactory.getCheckWiki();
    Page page = DataManager.getPage(wiki, title, null, null, null);
    List<CheckWikiDetection> detections = checkWiki.check(page);
    if (detections == null) {
      Utilities.displayWarning(
          parent,
          GT._T("Unable to retrieve analysis from CheckWiki."));
      return;
    }
    if (detections.isEmpty()) {
      Utilities.displayInformationMessage(
          parent,
          GT._T("No errors are currently detected by CheckWiki."));
      return;
    }
    DetectionPanel panel = new DetectionPanel(
        detections, textPane);
    JOptionPane.showMessageDialog(
        parent, panel, GT._T("Detections"),
        JOptionPane.INFORMATION_MESSAGE);
  }
}
