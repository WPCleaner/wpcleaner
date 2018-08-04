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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import javax.swing.text.BadLocationException;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.constants.WPCConfigurationStringList;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.dataaccess.PageProvider;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.gui.swing.menu.BasicMenuCreator;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;


/**
 * Manage actions for inserting predefined texts in a text pane.
 */
public class ActionInsertPredefinedText implements ActionListener {

  /**
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  private static JButton createInternalButton(
      boolean showIcon) {
    String iconName = "gnome-insert-text.png";
    String label = GT._T("Insert text");
    return Utilities.createJButton(
        showIcon ? iconName : null,
        EnumImageSize.NORMAL,
        label, !showIcon,
        null);
  }

  /**
   * Create a button for inserting predefined texts in a text pane.
   * 
   * @param pane Text pane.
   * @param pageProvider Page provider.
   * @param listener Listener.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton createButton(
      MWPane pane, PageProvider pageProvider,
      ListenerPredefinedTextInsertion listener,
      boolean showIcon) {
    JButton button = createInternalButton(showIcon);
    button.addActionListener(new ActionInsertPredefinedText(pane, pageProvider, listener));
    return button;
  }

  /**
   * Add a button for inserting predefined texts in a text pane.
   * 
   * @param toolbar Tool bar.
   * @param pane Text pane.
   * @param pageProvider Page provider.
   * @param listener Listener.
   * @param showIcon True if the button should use an icon.
   * @return Button.
   */
  public static JButton addButton(
      JToolBar toolbar,
      MWPane pane, PageProvider pageProvider,
      ListenerPredefinedTextInsertion listener,
      boolean showIcon) {
    JButton button = createButton(pane, pageProvider, listener, showIcon);
    if ((button != null) && (toolbar != null)) {
      toolbar.add(button);
    }
    return button;
  }

  /** Text pane on which the action should be applied. */
  private final MWPane pane;

  /** Page provider. */
  private final PageProvider pageProvider;

  /** Listener. */
  private final ListenerPredefinedTextInsertion listener;

  /**
   * @param pane Text pane.
   * @param pageProvider Page provider.
   * @param listener Listener.
   */
  private ActionInsertPredefinedText(
      MWPane pane, PageProvider pageProvider,
      ListenerPredefinedTextInsertion listener) {
    this.pane = pane;
    this.pageProvider = pageProvider;
    this.listener = listener;
  }

  /**
   * Show menu for selecting which text to insert.
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
    if ((pane == null) || (pageProvider == null)) {
      return;
    }
    Page page = pageProvider.getPage();
    if ((page == null) || (page.getContents() == null)) {
      return;
    }

    // Check current page
    boolean article = (page.isArticle());
    boolean redirect = article && page.isRedirect();

    // Check configuration
    WPCConfiguration wpcConfig = page.getWikipedia().getConfiguration();
    List<String> texts = wpcConfig.getStringList(
        WPCConfigurationStringList.INSERT_TEXTS);
    List<String> redirectCategories = null;
    List<String> redirectTemplates = null;
    if (redirect) {
      redirectCategories = wpcConfig.getStringList(
          WPCConfigurationStringList.REDIRECT_CATEGORIES);
      redirectTemplates = wpcConfig.getStringList(
          WPCConfigurationStringList.REDIRECT_TEMPLATES);
    }
    if (((texts == null) || (texts.isEmpty())) &&
        ((redirectCategories == null) || (redirectCategories.isEmpty())) &&
        ((redirectTemplates == null) || (redirectTemplates.isEmpty()))) {
      List<String> params = new ArrayList<String>();
      params.add(WPCConfigurationStringList.INSERT_TEXTS.getAttributeName());
      if (redirect) {
        params.add(WPCConfigurationStringList.REDIRECT_CATEGORIES.getAttributeName());
        params.add(WPCConfigurationStringList.REDIRECT_TEMPLATES.getAttributeName());
      }
      Utilities.displayMessageForMissingConfiguration(
          pane.getRootPane(), params);
      return;
    }

    // Group by theme if available
    Map<String, JMenu> themeMenus = new HashMap<String, JMenu>();

    // Create menu
    List<JMenuItem> items = new ArrayList<JMenuItem>();
    if (texts != null) {
      for (String text : texts) {
        int pipeIndex = text.indexOf('|');
        JMenu themeMenu = null;
        if (pipeIndex > 0) {
          String theme = text.substring(0, pipeIndex);
          text = text.substring(pipeIndex + 1);
          themeMenu = themeMenus.get(theme);
          if (themeMenu == null) {
            themeMenu = new JMenu(theme);
            themeMenus.put(theme, themeMenu);
            items.add(themeMenu);
          }
        }
        pipeIndex = text.indexOf('|');
        String label = null;
        if (pipeIndex > 0) {
          label = text.substring(0, pipeIndex);
          text = text.substring(pipeIndex + 1);
        }
        JMenuItem item = new JMenuItem((label != null) ? label : text);
        item.setActionCommand(text.replaceAll("\\\\n", "\n"));
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionAddText", "actionCommand"));
        if (themeMenu == null) {
          items.add(item);
        } else {
          themeMenu.add(item);
        }
      }
    }
    if (redirectCategories != null) {
      for (String category : redirectCategories) {
        int colonIndex = category.indexOf(':');
        JMenu themeMenu = null;
        if (colonIndex > 0) {
          String theme = category.substring(0, colonIndex);
          category = category.substring(colonIndex + 1);
          themeMenu = themeMenus.get(theme);
          if (themeMenu == null) {
            themeMenu = new JMenu(theme);
            themeMenus.put(theme, themeMenu);
            items.add(themeMenu);
          }
        }
        JMenuItem item = new JMenuItem(category);
        item.setActionCommand(category);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionAddCategory", "actionCommand"));
        if (themeMenu == null) {
          items.add(item);
        } else {
          themeMenu.add(item);
        }
      }
    }
    if (redirectTemplates != null) {
      for (String template : redirectTemplates) {
        int colonIndex = template.indexOf(':');
        JMenu themeMenu = null;
        if (colonIndex > 0) {
          String theme = template.substring(0, colonIndex);
          template = template.substring(colonIndex + 1);
          themeMenu = themeMenus.get(theme);
          if (themeMenu == null) {
            themeMenu = new JMenu(theme);
            themeMenus.put(theme, themeMenu);
            items.add(themeMenu);
          }
        }
        JMenuItem item = new JMenuItem("{{" + template + "}}");
        item.setActionCommand(template);
        item.addActionListener(EventHandler.create(
            ActionListener.class, this, "actionAddTemplate", "actionCommand"));
        if (themeMenu == null) {
          items.add(item);
        } else {
          themeMenu.add(item);
        }
      }
    }
    BasicMenuCreator menu = new BasicMenuCreator();
    JPopupMenu popup = menu.createPopupMenu(null);
    menu.addSubmenus(popup, items);
    popup.show(source, 0, source.getHeight());
  }

  /**
   * Action called when a text is selected to be added.
   * 
   * @param text Text.
   */
  public void actionAddText(String text) {
    if ((text == null) || (pageProvider == null)) {
      return;
    }
    Page page = pageProvider.getPage();
    if (page == null) {
      return;
    }
    try {
      API api = APIFactory.getAPI();
      text = api.parseText(pageProvider.getWiki(), page.getTitle(), text, false);
    } catch (APIException e) {
      // Nothing to do
    }
    try {
      pane.getDocument().insertString(
          pane.getCaretPosition(), text, null);
    } catch (BadLocationException e) {
      // Nothing to do
    }
  }

  /**
   * Action called when a category is selected to be added.
   * 
   * @param categoryName Category name.
   */
  public void actionAddCategory(String categoryName) {
    if ((categoryName == null) || (pageProvider == null)) {
      return;
    }
    Page page = pageProvider.getPage();
    if (page == null) {
      return;
    }
    String contents = pane.getText();
    PageAnalysis analysis = page.getAnalysis(contents, false);

    // Check that the category isn't already applied
    List<PageElementCategory> categories = analysis.getCategories();
    for (PageElementCategory category : categories) {
      if (Page.areSameTitle(categoryName, category.getCategory())) {
        return;
      }
    }

    // Find where to add the category
    int index = contents.length();
    if (!categories.isEmpty()) {
      index = categories.get(categories.size() - 1).getEndIndex();
    } else {
      List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
      if ((langLinks != null) && (!langLinks.isEmpty())) {
        index = langLinks.get(0).getBeginIndex();
      }
    }

    // Add the category
    StringBuilder newContents = new StringBuilder();
    if (index > 0) {
      newContents.append(contents.substring(0, index));
    }
    newContents.append("\n");
    newContents.append(PageElementCategory.createCategory(
        page.getWikipedia(), categoryName, null));
    if (index < contents.length()) {
      if (contents.charAt(index) != '\n') {
        newContents.append('\n');
      }
      newContents.append(contents.substring(index));
    }
    pane.changeText(newContents.toString());
    if (listener != null) {
      listener.templateInserted(categoryName);
    }
  }

  /**
   * Action called when a template is selected to be added.
   * 
   * @param templateName Template name.
   */
  public void actionAddTemplate(String templateName) {
    if ((templateName == null) || (pageProvider == null)) {
      return;
    }
    Page page = pageProvider.getPage();
    if (page == null) {
      return;
    }
    String contents = pane.getText();
    PageAnalysis analysis = page.getAnalysis(contents, false);

    // Check that the template isn't already applied
    List<PageElementTemplate> templates = analysis.getTemplates(templateName);
    if ((templates != null) && (!templates.isEmpty())) {
      return;
    }

    // Find where to add the template
    int crBefore = 0;
    int crAfter = 2;
    int index = contents.length();
    templates = analysis.getTemplates();
    if ((templates != null) && (!templates.isEmpty())) {
      index = templates.get(0).getBeginIndex();
      crAfter = 1;
      int indexNewLine = contents.indexOf('\n');
      if ((indexNewLine > 0) && (indexNewLine > index)) {
        crBefore = 2;
      }
    } else {
      List<PageElementCategory> categories = analysis.getCategories();
      if ((categories != null) && (!categories.isEmpty())) {
        index = categories.get(0).getBeginIndex();
      } else {
        List<PageElementLanguageLink> langLinks = analysis.getLanguageLinks();
        if ((langLinks != null) && (!langLinks.isEmpty())) {
          index = langLinks.get(0).getBeginIndex();
        } else {
          int indexNewLine = contents.indexOf('\n');
          if (indexNewLine > 0) {
            index = indexNewLine;
          }
          crBefore = 2;
          crAfter = 0;
        }
      }
    }

    // Add the template
    StringBuilder newContents = new StringBuilder();
    if (index > 0) {
      newContents.append(contents.substring(0, index));
    }
    for (int i = 0; i < crBefore; i++) {
      newContents.append("\n");
    }
    newContents.append("{{");
    newContents.append(templateName);
    newContents.append("}}");
    for (int i = 0; i < crAfter; i++) {
      newContents.append("\n");
    }
    if (index < contents.length()) {
      newContents.append(contents.substring(index));
    }
    pane.changeText(newContents.toString());
    if (listener != null) {
      listener.templateInserted(templateName);
    }
  }
}
