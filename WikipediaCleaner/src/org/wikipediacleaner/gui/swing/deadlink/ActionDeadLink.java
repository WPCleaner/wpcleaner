/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.gui.swing.deadlink;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.dataaccess.PageListProvider;
import org.wikipediacleaner.api.dataaccess.StaticPageListProvider;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for checking dead links in an article.
 */
public class ActionDeadLink extends AbstractAction implements ActionListener {

  /** Serialization */
  private static final long serialVersionUID = -5691508590320375863L;

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    return Utilities.createJButton(
        showIcon ? "commons-icons8-flat-broken-link.png" : null,
        EnumImageSize.NORMAL,
        GT._T("Check dead links"), !showIcon,
        null);
  }

  /**
   * Create a button for checking an article.
   * 
   * @param window Window.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      BasicWindow window, EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionDeadLink(window, wiki, title, textPane));
    return button;
  }

  /**
   * Create a button for checking a list of articles.
   * 
   * @param window Window.
   * @param wiki Wiki.
   * @param pageListProvider Provider for the list of pages.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      BasicWindow window, EnumWikipedia wiki,
      PageListProvider pageListProvider,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionDeadLink(window, wiki, pageListProvider));
    return button;
  }

  /**
   * Add a button for checking an article.
   * 
   * @param window Window.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      BasicWindow window, JToolBar toolbar,
      EnumWikipedia wiki,
      String title, JTextComponent textPane,
      boolean showIcon) {
    JButton button = createButton(window, wiki, title, textPane, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Add a button for checking a list of articles.
   * 
   * @param window Window.
   * @param toolbar Tool bar.
   * @param wiki Wiki.
   * @param pageListProvider Provider for the list of pages.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      BasicWindow window, JToolBar toolbar,
      EnumWikipedia wiki,
      PageListProvider pageListProvider,
      boolean showIcon) {
    JButton button = createButton(window, wiki, pageListProvider, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Wiki */
  private final EnumWikipedia wiki;

  /** Text pane where the text is */
  private final JTextComponent textPane;

  /** Window */
  private final BasicWindow window;

  /** Page list provider */
  private final PageListProvider pageListProvider;

  /**
   * Constructor for checking one article.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param title Page title.
   * @param textPane Text pane where the text is.
   */
  private ActionDeadLink(
      BasicWindow window, EnumWikipedia wiki,
      String title, JTextComponent textPane) {
    this.wiki = wiki;
    this.textPane = textPane;
    this.window = window;
    this.pageListProvider = new StaticPageListProvider(wiki, title);
  }

  /**
   * Constructor for checking a list of articles.
   * 
   * @param parent Parent component.
   * @param window Window.
   * @param wiki Wiki.
   * @param pageListProvider Page list provider.
   */
  private ActionDeadLink(
      BasicWindow window, EnumWikipedia wiki,
      PageListProvider pageListProvider) {
    this.wiki = wiki;
    this.textPane = null;
    this.window = window;
    this.pageListProvider = pageListProvider;
  }

  /**
   * Perform the action (check one page or a list of pages).
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    processPageList();
  }

  /**
   * Check a list of pages.
   */
  private void processPageList() {
    if (pageListProvider == null) {
      return;
    }
    List<Page> pages = pageListProvider.getPages();
    Component parent = (window != null) ? window.getParentComponent() : null;
    if ((pages == null) || (pages.isEmpty())) {
      Utilities.displayError(
          parent,
          GT._T("You need to select pages to check for dead links"));
      return;
    }
    Utilities.displayYesNoWarning(parent, GT._T("Do you want to check for dead links?"));
    DeadLinkWorker worker = new DeadLinkWorker(wiki, window, pages, textPane);
    worker.start();
  }
}
