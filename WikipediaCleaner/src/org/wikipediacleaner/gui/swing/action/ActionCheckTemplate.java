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

import javax.swing.JMenuItem;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.gui.swing.worker.CheckArticleTools;
import org.wikipediacleaner.i18n.GT;


/**
 * Manage actions for checking a template.
 */
public class ActionCheckTemplate implements ActionListener {

  /**
   * Create a menu item for checking a template.
   * 
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param template Template.
   * @return Menu item.
   */
  public static JMenuItem createMenuItem(
      Component parent,
      EnumWikipedia wiki, PageElementTemplate template) {
    JMenuItem menuItem = Utilities.createJMenuItem(GT._T("Check template"), true);
    menuItem.addActionListener(new ActionCheckTemplate(parent, wiki, template));
    return menuItem;
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
   * Template to be checked.
   */
  private final PageElementTemplate template;

  /**
   * @param parent Parent component.
   * @param wiki Wiki.
   * @param template Template to be checked.
   */
  private ActionCheckTemplate(
      Component parent,
      EnumWikipedia wiki, PageElementTemplate template) {
    this.parent = parent;
    this.wiki = wiki;
    this.template = template;
  }

  /**
   * Check a template.
   * 
   * @param e Event triggering this call.
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if ((e == null) || (wiki == null) || (template == null)) {
      return;
    }

    CheckArticleTools tools = new CheckArticleTools(wiki);
    try {
      tools.checkTemplate(template);
    } catch (APIException exception) {
      return;
    }
    String report = tools.getReport();
    Utilities.displayWarning(parent, report);
  }
}
