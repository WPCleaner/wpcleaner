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
import java.beans.EventHandler;
import java.util.Collections;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.text.JTextComponent;

import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationString;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.dataaccess.PageListProvider;
import org.wikipediacleaner.api.dataaccess.PageProvider;
import org.wikipediacleaner.api.dataaccess.WikiProvider;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.UpdateDabWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateDuplicateArgsWarningWorker;
import org.wikipediacleaner.gui.swing.worker.UpdateISBNWarningWorker;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Manage actions for updating warnings (disambiguation, ISBN, ...).
 */
public class ActionUpdateWarning implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button
   */
  private static JButton createInternalButton(
      boolean showIcon, boolean showText) {
    return Utilities.createJButton(
        showIcon ? "gnome-dialog-warning.png" : null,
        EnumImageSize.NORMAL,
        GT._T("Add warnings on talk page"), showText,
        null);
  }

  /**
   * Create a button for updating warnings.
   * 
   * @param parent Parent component.
   * @param window Window.
   * @param pageProvider Page provider.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, BasicWindow window,
      PageProvider pageProvider,
      boolean showIcon, boolean showText) {
    JButton button = createInternalButton(showIcon, showText);
    button.addActionListener(new ActionUpdateWarning(
        parent, window, pageProvider));
    return button;
  }

  /**
   * Add a button for updating warnings.
   * 
   * @param parent Parent component.
   * @param window Window.
   * @param toolbar Tool bar.
   * @param pageProvider Page provider.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, BasicWindow window,
      JToolBar toolbar,
      PageProvider pageProvider,
      boolean showIcon, boolean showText) {
    JButton button = createButton(
        parent, window, pageProvider, showIcon, showText);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /**
   * Create a button for updating warnings on selected pages.
   * 
   * @param parent Parent component.
   * @param window Window.
   * @param pageListProvider Page list provider.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton createButton(
      Component parent, BasicWindow window,
      PageListProvider pageListProvider,
      boolean showIcon, boolean showText) {
    JButton button = createInternalButton(showIcon, showText);
    button.addActionListener(new ActionUpdateWarning(
        parent, window, pageListProvider));
    return button;
  }

  /**
   * Create a button for updating warnings on selected pages.
   * 
   * @param parent Parent component.
   * @param window Window.
   * @param toolbar Tool bar.
   * @param pageListProvider Page list provider.
   * @param showIcon True if the button should use an icon.
   * @param showText True if the button should display the text.
   * @return Button.
   */
  public static JButton addButton(
      Component parent, BasicWindow window, JToolBar toolbar,
      PageListProvider pageListProvider,
      boolean showIcon, boolean showText) {
    JButton button = createButton(parent, window, pageListProvider, showIcon, showText);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Parent component. */
  private final Component parent;

  /** Window */
  private final BasicWindow window;

  /** Wiki for which warnings should be updated. */
  private final WikiProvider wikiProvider;

  /** Page for which warnings should be updated. */
  private final PageProvider pageProvider;

  /** List of pages for which warnings should be updated. */
  private final PageListProvider pageListProvider;

  /** Text component containing the page name. */
  private final JTextComponent text;

  /** Combo box containing the page name. */
  private final JComboBox combo;

  /**
   * @param parent Parent component.
   * @param window Window.
   * @param pageProvider Page for which warnings should be updated.
   */
  public ActionUpdateWarning(
      Component parent, BasicWindow window,
      PageProvider pageProvider) {
    this.parent = parent;
    this.window = window;
    this.wikiProvider = pageProvider;
    this.pageProvider = pageProvider;
    this.pageListProvider = null;
    this.text = null;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param window Window.
   * @param pageListProvider List of pages for which warnings should be updated.
   */
  public ActionUpdateWarning(
      Component parent, BasicWindow window,
      PageListProvider pageListProvider) {
    this.parent = parent;
    this.window = window;
    this.wikiProvider = pageListProvider;
    this.pageProvider = null;
    this.pageListProvider = pageListProvider;
    this.text = null;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param window Window.
   * @param text Text component containing the page name.
   */
  public ActionUpdateWarning(
      Component parent, BasicWindow window,
      JTextComponent text) {
    this.parent = parent;
    this.window = window;
    this.wikiProvider = null;
    this.pageProvider = null;
    this.pageListProvider = null;
    this.text = text;
    this.combo = null;
  }

  /**
   * @param parent Parent component.
   * @param window Window.
   * @param combo Combo box containing the page name.
   */
  public ActionUpdateWarning(
      Component parent, BasicWindow window,
      JComboBox combo) {
    this.parent = parent;
    this.window = window;
    this.wikiProvider = null;
    this.pageProvider = null;
    this.pageListProvider = null;
    this.text = null;
    this.combo = combo;
  }

  /**
   * Show menu for updating warnings.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) ||
        (e.getSource() == null) ||
        (!(e.getSource() instanceof Component))) {
      return;
    }
    Component source = (Component) e.getSource();

    // Display menu
    JPopupMenu menuWarning = new JPopupMenu();
    JMenuItem menuItem = Utilities.createJMenuItem(
        GT._T("Add a warning about links to disambiguation pages"),
        true);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDisambiguationWarning"));
    menuWarning.add(menuItem);

    menuItem = Utilities.createJMenuItem(
        GT._T("Add a warning about ISBN errors"),
        true);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionISBNWarning"));
    menuWarning.add(menuItem);

    menuItem = Utilities.createJMenuItem(
        GT._T("Add a warning about duplicate arguments errors"),
        true);
    menuItem.addActionListener(EventHandler.create(
        ActionListener.class, this, "actionDuplicateArgsWarning"));
    menuWarning.add(menuItem);

    menuWarning.show(source, 0, source.getHeight());
  }

  /**
   * Update disambiguation warnings on talk page.
   */
  public void actionDisambiguationWarning() {

    // Check selection
    if (wikiProvider == null) {
      return;
    }
    EnumWikipedia wiki = wikiProvider.getWiki();
    if (wiki == null) {
      return;
    }
    List<Page> pages = getPages();
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }

    // Check configuration
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.DAB_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          parent,
          WPCConfigurationString.DAB_WARNING_TEMPLATE.getAttributeName());
      return;
    }

    // Ask for confirmation
    int answer = Utilities.displayYesNoWarning(
        parent,
        GT._T("Do you want to update the disambiguation warning on talk page?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }

    // Update warning
    UpdateDabWarningWorker worker = new UpdateDabWarningWorker(
        wiki, window, pages,
        false, false, false, false);
    worker.start();
  }

  /**
   * Update ISBN warnings on talk page.
   */
  public void actionISBNWarning() {

    // Check selection
    if (wikiProvider == null) {
      return;
    }
    EnumWikipedia wiki = wikiProvider.getWiki();
    if (wiki == null) {
      return;
    }
    List<Page> pages = getPages();
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }

    // Check configuration
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.ISBN_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          parent,
          WPCConfigurationString.ISBN_WARNING_TEMPLATE.getAttributeName());
      return;
    }

    // Ask for confirmation
    int answer = Utilities.displayYesNoWarning(
        parent,
        GT._T("Do you want to update the ISBN warning on talk page?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }

    // Update warning
    UpdateISBNWarningWorker worker = new UpdateISBNWarningWorker(
        wiki, window, pages,
        false, false);
    worker.start();
  }

  /**
   * Update duplicate arguments warnings on talk page.
   */
  public void actionDuplicateArgsWarning() {

    // Check selection
    if (wikiProvider == null) {
      return;
    }
    EnumWikipedia wiki = wikiProvider.getWiki();
    if (wiki == null) {
      return;
    }
    List<Page> pages = getPages();
    if ((pages == null) || (pages.isEmpty())) {
      return;
    }

    // Check configuration
    WPCConfiguration wpcConfig = wiki.getConfiguration();
    String template = wpcConfig.getString(WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE);
    if ((template == null) || (template.trim().length() == 0)) {
      Utilities.displayMessageForMissingConfiguration(
          parent,
          WPCConfigurationString.DUPLICATE_ARGS_WARNING_TEMPLATE.getAttributeName());
      return;
    }

    // Ask for confirmation
    int answer = Utilities.displayYesNoWarning(
        parent,
        GT._T("Do you want to update the duplicate arguments warning on talk page?"));
    if (answer != JOptionPane.YES_OPTION) {
      return;
    }

    // Update warning
    UpdateDuplicateArgsWarningWorker worker = new UpdateDuplicateArgsWarningWorker(
        wiki, window, pages,
        false, false);
    worker.start();
  }

  /**
   * @return List of pages for which warnings should be updated.
   */
  private List<Page> getPages() {

    // Manage list of pages
    if (pageListProvider != null) {
      return pageListProvider.getPages();
    }

    // Manage a single page
    if (pageProvider != null) {
      Page page = pageProvider.getPage();
      if (page != null) {
        return Collections.singletonList(page);
      }
    }

    // Manage page name in a text field or combo box
    EnumWikipedia wiki = (wikiProvider != null) ? wikiProvider.getWiki() : null;
    if (((text != null) || (combo != null)) && (wiki != null)) {
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
            GT._T("You must input a page name for updating warnings"),
            (text != null) ? text : combo);
        return null;
      }
      tmp = tmp.trim();
      Configuration config = Configuration.getConfiguration();
      config.setString(
          null, ConfigurationValueString.PAGE_NAME, tmp);
      config.save();
      return Collections.singletonList(
          DataManager.getPage(wiki, tmp, null, null, null));
    }

    return null;
  }
}
