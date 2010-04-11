/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.gui.swing.component;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.event.ActionListener;
import java.awt.font.TextAttribute;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Properties;

import javax.swing.JCheckBox;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTextPane;
import javax.swing.text.Element;

import org.wikipediacleaner.api.check.Actionnable;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageUtilities;
import org.wikipediacleaner.api.data.TemplateMatch;
import org.wikipediacleaner.api.data.TemplateParameter;
import org.wikipediacleaner.api.data.TemplateReplacement;
import org.wikipediacleaner.gui.swing.action.DisambiguationAnalysisAction;
import org.wikipediacleaner.gui.swing.action.FindTextAction;
import org.wikipediacleaner.gui.swing.action.FullPageAnalysisAction;
import org.wikipediacleaner.gui.swing.action.MarkLinkAction;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
import org.wikipediacleaner.gui.swing.action.PurgeCacheAction;
import org.wikipediacleaner.gui.swing.action.RedLinksAnalysisAction;
import org.wikipediacleaner.gui.swing.action.ReloadLinksAction;
import org.wikipediacleaner.gui.swing.action.RemoveAllLinksAction;
import org.wikipediacleaner.gui.swing.action.RemoveLinkAction;
import org.wikipediacleaner.gui.swing.action.ReplaceAllLinksAction;
import org.wikipediacleaner.gui.swing.action.ReplaceLinkAction;
import org.wikipediacleaner.gui.swing.action.ReplaceTemplateAction;
import org.wikipediacleaner.gui.swing.action.RevertLinkAction;
import org.wikipediacleaner.gui.swing.action.TemplatesAnalysisAction;
import org.wikipediacleaner.gui.swing.basic.BasicWindow;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A helper class to create menus.
 */
public class MenuCreator {

  private final static HashMap<String, String> lastReplacement = new HashMap<String, String>();

  final private static HashMap<TextAttribute, Color> disambiguationAttributes = new HashMap<TextAttribute, Color>();
  final private static HashMap<TextAttribute, Boolean> missingAttributes = new HashMap<TextAttribute, Boolean>();

  final private static Configuration configuration = Configuration.getConfiguration();

  static {
    Properties tmp = configuration.getProperties(
        Configuration.PROPERTIES_LAST_REPLACEMENT);
    if (tmp != null) {
      for (Object object : tmp.keySet()) {
        if (object instanceof String) {
          lastReplacement.put((String) object, tmp.getProperty((String) object, "")); 
        }
      }
    }
    disambiguationAttributes.put(TextAttribute.FOREGROUND, Color.RED);
    missingAttributes.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);
  }

  /**
   * Memorize a replacement.
   * 
   * @param from From.
   * @param to To.
   */
  public static void addLastReplacement(String from, String to) {
    if ((from != null) && (to != null)) {
      lastReplacement.put(from, to);
      if (configuration.getBoolean(
          Configuration.BOOLEAN_SAVE_LAST_REPLACEMENT,
          Configuration.DEFAULT_SAVE_LAST_REPLACEMENT)) {
        configuration.setSubString(
            Configuration.PROPERTIES_LAST_REPLACEMENT,
            from, to);
      }
    }
  }

  /**
   * Get a replacement.
   * 
   * @param from From.
   * @return Replacement.
   */
  public static String getLastReplacement(String from) {
    if (from != null) {
      return lastReplacement.get(from);
    }
    return null;
  }

  public static void addInfoToMenu(
      JPopupMenu popup, Element element, JTextPane textPane,
      int position, CheckErrorResult info) {
    if ((popup == null) || (element == null) || (textPane == null) || (info == null)) {
      return;
    }
    
    // Current chapter
    JMenuItem menuItem = new JMenuItem(info.getErrorType());
    menuItem.setEnabled(false);
    popup.add(menuItem);
    addCurrentChapterToMenu(popup, textPane, position);
    popup.add(new JSeparator());

    // Actions
    ArrayList<Actionnable> possibleActions = info.getPossibleActions(element, textPane);
    if (possibleActions != null) {
      for (Actionnable possibleAction : possibleActions) {
        if (possibleAction.isCompositeAction()) {
          JMenu subMenu = new JMenu(possibleAction.getName());
          for (Actionnable subAction : possibleAction.getActions()) {
            menuItem = new JMenuItem(subAction.getName());
            menuItem.addActionListener(subAction.getAction());
            subMenu.add(menuItem);
          }
          addSubmenu(popup, subMenu, 0, 0);
        } else {
          menuItem = new JMenuItem(possibleAction.getName());
          menuItem.addActionListener(possibleAction.getAction());
          popup.add(menuItem);
        }
      }
    }
  }

  /**
   * Add submenus for showing current chapter organization.
   * 
   * @param popup Popup menu.
   * @param textPane Text pane (must be a {@link MediaWikiPane})
   * @param position Current position in text
   */
  public static void addCurrentChapterToMenu(
      JPopupMenu popup, JTextPane textPane, int position) {
    if ((popup == null) || !(textPane instanceof MediaWikiPane)) {
      return;
    }
    MediaWikiPane pane = (MediaWikiPane) textPane;
    LinkedList<String> chapters = pane.getChapterPosition(position);
    if ((chapters != null) && !chapters.isEmpty()) {
      JMenu submenu = new JMenu(GT._("Current chapter"));
      for (String chapter : chapters) {
        JMenuItem menuItem = new JMenuItem(chapter);
        menuItem.setEnabled(false);
        submenu.add(menuItem);
      }
      addSubmenu(popup, submenu, 0, 0);
    }
  }

  /**
   * Add submenus for replacing templates.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param template Template name.
   * @param params Templates parameters.
   * @param disambigPage Disambiguation page.
   * @param page Page containing the text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public static void addReplaceTemplateToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, String template, String params,
      Page disambigPage, Page page, Element element, JTextPane textPane) {
    if ((popup == null) ||
        (template == null) ||
        (params == null) ||
        (wikipedia == null)) {
      return;
    }
    if (!Boolean.TRUE.equals(disambigPage.isDisambiguationPage())) {
      return;
    }
    TemplateMatch tm = wikipedia.getDisambiguationMatch(template);
    if (tm == null) {
      return;
    }
    ArrayList<TemplateParameter> templateParams = PageUtilities.analyzeTemplateParameters(tm, params, page);
    for (TemplateParameter param : templateParams) {
      if (Page.areSameTitle(param.getValue(), disambigPage.getTitle())) {
        TemplateReplacement replacement = tm.getReplacement(param.getName());
        if (replacement != null) {
          JMenuItem menuItem = null;
          ActionListener action = null;
          ArrayList<Page> links = disambigPage.getLinksWithRedirect();
          JMenu submenuLink = new JMenu(GT._("Link parameter {0} to", param.getName()));
          JMenu submenuReplace = new JMenu(GT._("Replace parameter {0} with", param.getName()));

          // Determine if separators are needed
          boolean separators = false;
          if ((links != null) && (links.size() > 0)) {
            separators = true;
          }

          // Last replacement
          int fixedBeginLink = 0;
          int fixedEndLink = 0;
          int fixedBeginReplace = 0;
          int fixedEndReplace = 0;
          String title = getLastReplacement(disambigPage.getTitle());
          if (title != null) {
            menuItem = new JMenuItem(title);
            action = new ReplaceTemplateAction(
                tm.getName(), templateParams, replacement,
                disambigPage.getTitle(), title, element, textPane, false);
            menuItem.addActionListener(action);
            menuItem.setAccelerator(MediaWikiPane.getLastLinkKeyStroke());
            submenuLink.add(menuItem);
            fixedBeginLink++;
            menuItem = new JMenuItem(title);
            action = new ReplaceTemplateAction(
                tm.getName(), templateParams, replacement,
                disambigPage.getTitle(), title, element, textPane, true);
            menuItem.addActionListener(action);
            menuItem.setAccelerator(MediaWikiPane.getLastReplaceKeyStroke());
            submenuReplace.add(menuItem);
            fixedBeginReplace++;
          }

          // Separators
          if (separators) {
            fixedBeginLink += addSeparator(submenuLink);
            fixedBeginReplace += addSeparator(submenuReplace);
          }

          // Possible links
          if ((links != null) && (links.size() > 0)) {
            for (Page p : links) {
              if (p.isRedirect()) {
                JMenu submenu1 = new JMenu(p.getTitle());
                JMenu submenu2 = new JMenu(p.getTitle());
                
                Iterator<Page> iter = p.getRedirectIteratorWithPage();
                while (iter.hasNext()) {
                  Page pageTmp = iter.next();
                  
                  menuItem = new JMenuItem(pageTmp.getTitle());
                  updateFont(menuItem, pageTmp);
                  action = new ReplaceTemplateAction(
                      tm.getName(), templateParams, replacement,
                      disambigPage.getTitle(), pageTmp.getTitle(), element, textPane, false);
                  menuItem.addActionListener(action);
                  submenu1.add(menuItem);
          
                  menuItem = new JMenuItem(pageTmp.getTitle());
                  updateFont(menuItem, pageTmp);
                  action = new ReplaceTemplateAction(
                      tm.getName(), templateParams, replacement,
                      disambigPage.getTitle(), pageTmp.getTitle(), element, textPane, true);
                  menuItem.addActionListener(action);
                  submenu2.add(menuItem);
                }
                
                submenuLink.add(submenu1);
                submenuReplace.add(submenu2);
              } else {
                menuItem = new JMenuItem(p.getTitle());
                updateFont(menuItem, p);
                action = new ReplaceTemplateAction(
                    tm.getName(), templateParams, replacement,
                    disambigPage.getTitle(), p.getTitle(), element, textPane, false);
                menuItem.addActionListener(action);
                submenuLink.add(menuItem);
        
                menuItem = new JMenuItem(p.getTitle());
                updateFont(menuItem, p);
                action = new ReplaceTemplateAction(
                    tm.getName(), templateParams, replacement,
                    disambigPage.getTitle(), p.getTitle(), element, textPane, true);
                menuItem.addActionListener(action);
                submenuReplace.add(menuItem);
              }
            }
          }

          // Last replacement
          if (separators) {
            title = getLastReplacement(disambigPage.getTitle());
            if (title != null) {
              fixedEndLink += addSeparator(submenuLink);
              menuItem = new JMenuItem(title);
              action = new ReplaceTemplateAction(
                  tm.getName(), templateParams, replacement,
                  disambigPage.getTitle(), title, element, textPane, false);
              menuItem.addActionListener(action);
              menuItem.setAccelerator(MediaWikiPane.getLastLinkKeyStroke());
              submenuLink.add(menuItem);
              fixedEndLink++;
      
              fixedEndReplace += addSeparator(submenuReplace);
              menuItem = new JMenuItem(title);
              action = new ReplaceTemplateAction(
                  tm.getName(), templateParams, replacement,
                  disambigPage.getTitle(), title, element, textPane, true);
              menuItem.addActionListener(action);
              menuItem.setAccelerator(MediaWikiPane.getLastReplaceKeyStroke());
              submenuReplace.add(menuItem);
              fixedEndReplace++;
            }
          }

          addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
          addSubmenu(popup, submenuReplace, fixedBeginReplace, fixedEndReplace);
        }
      }
    }
  }

  /**
   * Add submenus for replacing links.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public static void addReplaceLinkToMenu(
      JPopupMenu popup, Page page, String text, Element element, JTextPane textPane) {
    if (text == null) {
      return;
    }
    JMenuItem menuItem = null;
    ActionListener action = null;

    // Disambiguation page
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      ArrayList<Page> links = page.getLinksWithRedirect();
      JMenu submenuLink = new JMenu(GT._("Link to"));
      JMenu submenuReplace = new JMenu(GT._("Replace with"));

      // Determine if separators are needed
      ArrayList<String> wiktionary = page.getWiktionaryLinks();
      boolean separators = false;
      if (((links != null) && (links.size() > 0)) ||
          ((wiktionary != null) && (wiktionary.size() > 0))) {
        separators = true;
      }

      // Last replacement
      int fixedBeginLink = 0;
      int fixedEndLink = 0;
      int fixedBeginReplace = 0;
      int fixedEndReplace = 0;
      String title = getLastReplacement(page.getTitle());
      if (title != null) {
        menuItem = new JMenuItem(title);
        action = new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, false);
        menuItem.addActionListener(action);
        menuItem.setAccelerator(MediaWikiPane.getLastLinkKeyStroke());
        submenuLink.add(menuItem);
        fixedBeginLink++;
        menuItem = new JMenuItem(title);
        action = new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true);
        menuItem.addActionListener(action);
        menuItem.setAccelerator(MediaWikiPane.getLastReplaceKeyStroke());
        submenuReplace.add(menuItem);
        fixedBeginReplace++;
      }

      // Separators
      if (separators) {
        fixedBeginLink += addSeparator(submenuLink);
        fixedBeginReplace += addSeparator(submenuReplace);
      }

      // Wiktionary links
      if ((wiktionary != null) && (wiktionary.size() > 0)) {
        for (String wikt : wiktionary) {
          String name = "wikt:" + wikt;
          menuItem = new JMenuItem(name);
          action = new ReplaceLinkAction(page.getTitle(), ":" + name, text, element, textPane, false);
          menuItem.addActionListener(action);
          submenuLink.add(menuItem);
        }
      }

      // Possible links
      if ((links != null) && (links.size() > 0)) {
        for (Page p : links) {
          if (p.isRedirect()) {
            JMenu submenu1 = new JMenu(p.getTitle());
            JMenu submenu2 = new JMenu(p.getTitle());
            
            Iterator<Page> iter = p.getRedirectIteratorWithPage();
            while (iter.hasNext()) {
              Page pageTmp = iter.next();
              
              menuItem = new JMenuItem(pageTmp.getTitle());
              updateFont(menuItem, pageTmp);
              action = new ReplaceLinkAction(page.getTitle(), pageTmp.getTitle(), text, element, textPane, false);
              menuItem.addActionListener(action);
              submenu1.add(menuItem);
      
              menuItem = new JMenuItem(pageTmp.getTitle());
              updateFont(menuItem, pageTmp);
              action = new ReplaceLinkAction(page.getTitle(), pageTmp.getTitle(), text, element, textPane, true);
              menuItem.addActionListener(action);
              submenu2.add(menuItem);
            }
            
            submenuLink.add(submenu1);
            submenuReplace.add(submenu2);
          } else {
            menuItem = new JMenuItem(p.getTitle());
            updateFont(menuItem, p);
            action = new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, false);
            menuItem.addActionListener(action);
            submenuLink.add(menuItem);
    
            menuItem = new JMenuItem(p.getTitle());
            updateFont(menuItem, p);
            action = new ReplaceLinkAction(page.getTitle(), p.getTitle(), text, element, textPane, true);
            menuItem.addActionListener(action);
            submenuReplace.add(menuItem);
          }
        }
      }

      // Last replacement
      if (separators) {
        title = getLastReplacement(page.getTitle());
        if (title != null) {
          fixedEndLink += addSeparator(submenuLink);
          menuItem = new JMenuItem(title);
          action = new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, false);
          menuItem.addActionListener(action);
          menuItem.setAccelerator(MediaWikiPane.getLastLinkKeyStroke());
          submenuLink.add(menuItem);
          fixedEndLink++;
  
          fixedEndReplace += addSeparator(submenuReplace);
          menuItem = new JMenuItem(title);
          action = new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true);
          menuItem.addActionListener(action);
          menuItem.setAccelerator(MediaWikiPane.getLastReplaceKeyStroke());
          submenuReplace.add(menuItem);
          fixedEndReplace++;
        }
      }

      addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
      addSubmenu(popup, submenuReplace, fixedBeginReplace, fixedEndReplace);

      if (!Page.areSameTitle(text, page.getTitle())) {
        menuItem = new JMenuItem(GT._("Reverse to [[{0}|{1}]]", new Object[]{text, page.getTitle()}));
        action = new RevertLinkAction(page.getTitle(), text, element, textPane);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }

    // Redirect page
    } else if (page.isRedirect()) {
      String title = page.getRedirectTitle();

      menuItem = Utilities.createJMenuItem(GT._("&Link to {0}", title));
      action = new ReplaceLinkAction(page.getTitle(), title, text,element, textPane, false);
      menuItem.addActionListener(action);
      popup.add(menuItem);

      menuItem = Utilities.createJMenuItem(GT._("&Replace with {0}", title));
      action = new ReplaceLinkAction(page.getTitle(), title, text, element, textPane, true);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for analysing missing page.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public static void addRedLinksAnalysisMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, MediaWikiPane textPane) {
    JMenuItem menuItem = new JMenuItem(GT._("Red links analysis"));
    ActionListener action = new RedLinksAnalysisAction(page, textPane, wikipedia);
    menuItem.addActionListener(action);
    menuItem.setEnabled(false); //TODO
    popup.add(menuItem);
  }

  /**
   * Add submenus for replacing links.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public static void addReplaceAllLinksToMenu(
      JPopupMenu popup, Page page, MediaWikiPane textPane) {
    JMenuItem menuItem = null;
    ActionListener action = null;
    ArrayList<Page> links = page.getLinksWithRedirect();
    if ((links != null) && (links.size() > 0)) {
      JMenu submenuLink = new JMenu(GT._("Link to"));
      //JMenu submenuReplace = new JMenu(GT._("Replace with"));

      int fixedBeginLink = 0;
      int fixedEndLink = 0;
      String title = getLastReplacement(page.getTitle());
      if (title != null) {
        menuItem = new JMenuItem(title);
        action = new ReplaceAllLinksAction(textPane, page, title);
        menuItem.addActionListener(action);
        submenuLink.add(menuItem);
        fixedBeginLink++;
        fixedBeginLink += addSeparator(submenuLink);
        //menuItem = new JMenuItem(title);
        //action = new FullyReplaceAllLinksAction(textPane, page, title);
        //menuItem.addActionListener(action);
        //submenuReplace.add(menuItem);
        //fixedBeginReplace += addSeparator(submenuReplace);
      }

      for (Page p : links) {
        if (p.isRedirect()) {
          JMenu submenu1 = new JMenu(p.getTitle());
          //JMenu submenu2 = new JMenu(p.getTitle());
          
          Iterator<Page> iter = p.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            
            menuItem = new JMenuItem(pageTmp.getTitle());
            updateFont(menuItem, pageTmp);
            action = new ReplaceAllLinksAction(textPane, page, pageTmp.getTitle());
            menuItem.addActionListener(action);
            submenu1.add(menuItem);
    
            //menuItem = new JMenuItem(pageTmp.getTitle());
            //updateFont(menuItem, pageTmp);
            //action = new FullyReplaceAllLinksAction(textPane, page, pageTmp.getTitle());
            //menuItem.addActionListener(action);
            //submenu2.add(menuItem);
          }
          
          submenuLink.add(submenu1);
          //submenuReplace.add(submenu2);
        } else {
          menuItem = new JMenuItem(p.getTitle());
          updateFont(menuItem, p);
          action = new ReplaceAllLinksAction(textPane, page, p.getTitle());
          menuItem.addActionListener(action);
          submenuLink.add(menuItem);
  
          //menuItem = new JMenuItem(p.getTitle());
          //updateFont(menuItem, p);
          //action = new FullyReplaceAllLinksAction(textPane, page, p.getTitle());
          //menuItem.addActionListener(action);
          //submenuReplace.add(menuItem);
        }
      }

      title = getLastReplacement(page.getTitle());
      if (title != null) {
        fixedEndLink += addSeparator(submenuLink);
        menuItem = new JMenuItem(title);
        action = new ReplaceAllLinksAction(textPane, page, title);
        menuItem.addActionListener(action);
        submenuLink.add(menuItem);
        fixedEndLink++;

        //fixedEndReplace += addSeparator(submenuReplace);
        //menuItem = new JMenuItem(title);
        //action = new FullyReplaceAllLinksAction(textPane, page, title);
        //menuItem.addActionListener(action);
        //submenuReplace.add(menuItem);
        //fixedEndReplace++;
      }

      addSubmenu(popup, submenuLink, fixedBeginLink, fixedEndLink);
      //addSubmenu(popup, submenuReplace, fixedBeginReplace, fixedEndReplace);
    }
  }

  /**
   * Add submenus for removing links.
   * 
   * @param popup Popup menu.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public static void addRemoveLinkToMenu(
      JPopupMenu popup, String text, Element element, JTextPane textPane) {
    if (text != null) {
      JMenuItem menuItem = new JMenuItem(GT._("Remove link"));
      ActionListener action = new RemoveLinkAction(text, element, textPane);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for removing links.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public static void addRemoveAllLinksToMenu(
      JPopupMenu popup, Page page, MediaWikiPane textPane) {
    JMenuItem menuItem = new JMenuItem(GT._("Remove all links"));
    ActionListener action = new RemoveAllLinksAction(textPane, page);
    menuItem.addActionListener(action);
    popup.add(menuItem);
  }

  /**
   * Add submenus for marking link as normal.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   */
  public static void addMarkAsNormalToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane) {
    String[] templates = (wikipedia != null) ? wikipedia.getTemplatesForDisambiguationLink() : null;
    if ((text != null) &&
        (page != null) &&
        Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        (templates != null) &&
        (templates.length > 0)) {
      if (templates.length > 1) {
        JMenu submenu = new JMenu(GT._("Mark as normal link"));
        for (String template : templates) {
          JMenuItem menuItem = new JMenuItem(GT._("Using '{{'{0}'}}'", template));
          ActionListener action = new MarkLinkAction(
              page.getTitle(), text, template,
              element, textPane, null);
          menuItem.addActionListener(action);
          submenu.add(menuItem);
        }
        popup.add(submenu);
      } else {
        JMenuItem menuItem = new JMenuItem(GT._("Mark as normal link"));
        ActionListener action = new MarkLinkAction(
            page.getTitle(), text, templates[0],
            element, textPane, null);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }
    }
  }

  /**
   * Add submenus for marking link as requiring help to disambiguate.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param text Text.
   * @param element Element.
   * @param textPane Text pane.
   * @param checkBox Check Box.
   */
  public static void addMarkAsNeedingHelpToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, String text,
      Element element, JTextPane textPane, JCheckBox checkBox) {
    String[] templates = (wikipedia != null) ? wikipedia.getTemplatesForNeedingHelp() : null;
    if ((text != null) &&
        (page != null) &&
        Boolean.TRUE.equals(page.isDisambiguationPage()) &&
        (templates != null) &&
        (templates.length > 0)) {
      if (templates.length > 1) {
        JMenu submenu = new JMenu(GT._("Mark as needing help"));
        for (String template : templates) {
          JMenuItem menuItem = new JMenuItem(GT._("Using '{{'{0}'}}'", template));
          ActionListener action = new MarkLinkAction(
              page.getTitle(), text, template,
              element, textPane, null);
          menuItem.addActionListener(action);
          submenu.add(menuItem);
        }
        popup.add(submenu);
      } else {
        JMenuItem menuItem = new JMenuItem(GT._("Mark as needing help"));
        ActionListener action = new MarkLinkAction(
            page.getTitle(), text, templates[0],
            element, textPane, checkBox);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }
    }
  }

  /**
   * Add submenus for analyzing pages.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   */
  public static void addAnalyzeToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page) {
    JMenuItem menuItem = null;
    ActionListener action = null;
    ArrayList<Page> links = page.getLinksWithRedirect();
    if (((links != null) && (links.size() > 0)) || page.isRedirect()) {
      int fixedBegin = 0;
      int fixedEnd = 0;
      JMenu submenuAnalyze = new JMenu(GT._("Analyze"));
      Iterator<Page> iter = page.getRedirectIteratorWithPage();
      while (iter.hasNext()) {
        Page pageTmp = iter.next();
        menuItem = new JMenuItem(pageTmp.getTitle());
        updateFont(menuItem, pageTmp);
        action = new FullPageAnalysisAction(pageTmp.getTitle(), wikipedia);
        menuItem.addActionListener(action);
        submenuAnalyze.add(menuItem);
        fixedBegin++;
      }
      if ((links != null) && (links.size() > 0)) {
        fixedBegin += addSeparator(submenuAnalyze);
  
        for (Page p : links) {
          menuItem = new JMenuItem(p.getTitle());
          updateFont(menuItem, p);
          action = new FullPageAnalysisAction(p.getTitle(), wikipedia);
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
        }

        fixedEnd += addSeparator(submenuAnalyze);

        iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          action = new FullPageAnalysisAction(pageTmp.getTitle(), wikipedia);
          menuItem.addActionListener(action);
          submenuAnalyze.add(menuItem);
          fixedEnd++;
        }
      }
      addSubmenu(popup, submenuAnalyze, fixedBegin, fixedEnd);
    } else {
      menuItem = new JMenuItem(GT._("Analyze page"));
      action = new FullPageAnalysisAction(page.getTitle(), wikipedia);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for viewing pages.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   */
  public static void addViewToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page) {
    JMenuItem menuItem = null;
    ActionListener action = null;
    ArrayList<Page> links = page.getLinksWithRedirect();
    if (Utilities.isDesktopSupported()) {
      if (((links != null) && (links.size() > 0)) || page.isRedirect()) {
        int fixedBegin = 0;
        int fixedEnd = 0;
        JMenu submenuView = new JMenu(GT._("External Viewer"));
        Iterator<Page> iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          action = new PageViewAction(pageTmp.getTitle(), wikipedia);
          menuItem.addActionListener(action);
          submenuView.add(menuItem);
          fixedBegin++;
        }
        if ((links != null) && (links.size() > 0)) {
          fixedBegin += addSeparator(submenuView);
    
          for (Page p : links) {
            if (p.isRedirect()) {
              JMenu submenuRedirect = new JMenu(p.getTitle());
              Iterator<Page> itPage = p.getRedirectIteratorWithPage();
              while (itPage.hasNext()) {
                Page redirect = itPage.next();
                menuItem = new JMenuItem(redirect.getTitle());
                updateFont(menuItem, redirect);
                action = new PageViewAction(redirect.getTitle(), wikipedia);
                menuItem.addActionListener(action);
                submenuRedirect.add(menuItem);
              }
              submenuView.add(submenuRedirect);
            } else {
              menuItem = new JMenuItem(p.getTitle());
              updateFont(menuItem, p);
              action = new PageViewAction(p.getTitle(), wikipedia);
              menuItem.addActionListener(action);
              submenuView.add(menuItem);
            }
          }

          fixedEnd += addSeparator(submenuView);

          iter = page.getRedirectIteratorWithPage();
          while (iter.hasNext()) {
            Page pageTmp = iter.next();
            menuItem = new JMenuItem(pageTmp.getTitle());
            updateFont(menuItem, pageTmp);
            action = new PageViewAction(pageTmp.getTitle(), wikipedia);
            menuItem.addActionListener(action);
            submenuView.add(menuItem);
            fixedEnd++;
          }
        }
        addSubmenu(popup, submenuView, fixedBegin, fixedEnd);
      } else {
        menuItem = new JMenuItem(GT._("External Viewer"));
        action = new PageViewAction(page.getTitle(), wikipedia);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      }
    }
  }

  /**
   * Add submenus for viewing pages.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param url URL.
   */
  public static void addViewToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, String url) {
    if (Utilities.isDesktopSupported()) {
      JMenuItem menuItem = new JMenuItem(GT._("External Viewer"));
      ActionListener action = new PageViewAction(url, wikipedia);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for disambiguation.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   */
  public static void addDisambiguationToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page) {
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      if (!page.isRedirect()) {
        JMenuItem menuItem = new JMenuItem(GT._("Disambiguation analysis"));
        ActionListener action = new DisambiguationAnalysisAction(page.getTitle(), wikipedia);
        menuItem.addActionListener(action);
        popup.add(menuItem);
      } else {
        JMenu submenuView = new JMenu(GT._("Disambiguation analysis"));
        Iterator<Page> iter = page.getRedirectIteratorWithPage();
        while (iter.hasNext()) {
          Page pageTmp = iter.next();
          JMenuItem menuItem = new JMenuItem(pageTmp.getTitle());
          updateFont(menuItem, pageTmp);
          ActionListener action = new DisambiguationAnalysisAction(pageTmp.getTitle(), wikipedia);
          menuItem.addActionListener(action);
          submenuView.add(menuItem);
        }
        popup.add(submenuView);
      }
    }
  }

  /**
   * Add submenus for reloading links. 
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public static void addReloadLinksToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, BasicWindow window) {
    if (page != null) {
      JMenuItem menuItem = new JMenuItem(GT._("Reload links"));
      ActionListener action = new ReloadLinksAction(wikipedia, page, window);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for purging page cache. 
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Page.
   * @param window Window.
   */
  public static void addPurgeCacheToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, BasicWindow window) {
    if (page != null) {
      JMenuItem menuItem = new JMenuItem(GT._("Purge cache"));
      ActionListener action = new PurgeCacheAction(wikipedia, page, window);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for finding text.
   * 
   * @param popup Popup menu.
   * @param page Page.
   * @param textPane Text pane.
   */
  public static void addFindTextToMenu(
      JPopupMenu popup, Page page, JTextPane textPane) {
    if ((page != null) && (textPane != null)) {
      JMenuItem menuItem = new JMenuItem(GT._("Find text"));
      ActionListener action = new FindTextAction(page.getTitle(), textPane);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add submenus for analyzing templates.
   * 
   * @param wikipedia Wikipedia.
   * @param popup Popup menu.
   * @param page Initial page.
   * @param link Link.
   */
  public static void addAnalyzeTemplatesToMenu(
      EnumWikipedia wikipedia, JPopupMenu popup, Page page, Page link) {
    if ((page != null) && (wikipedia != null)) {
      JMenuItem menuItem = new JMenuItem(GT._("Search in templates"));
      ActionListener action = new TemplatesAnalysisAction(page, link, wikipedia);
      menuItem.addActionListener(action);
      popup.add(menuItem);
    }
  }

  /**
   * Add a submenu to a menu.
   * If the submenu contains to much elements, it is split into several submenus.
   * 
   * @param menu Menu.
   * @param submenu Submenu.
   * @param begin Number of items kept at the beginning.
   * @param end Number of items kept at the end.
   */
  public static void addSubmenu(JPopupMenu menu, JMenu submenu, int begin, int end) {
    Configuration config = Configuration.getConfiguration();
    final int maxElements = Math.max(
        config.getInt(Configuration.INTEGER_MENU_SIZE, Configuration.DEFAULT_MENU_SIZE),
        begin + end + 2);
    if (submenu.getMenuComponentCount() > maxElements) {
      ArrayList<JMenu> menuList = new ArrayList<JMenu>();
      while (submenu.getMenuComponentCount() > begin + end + 1) {
        int count = Math.min(maxElements, submenu.getMenuComponentCount() - begin - end);
        JMenu newMenu = new JMenu(submenu.getItem(begin).getText() + "...");
        for (int i = 0; i < count; i++) {
          JMenuItem item = submenu.getItem(begin);
          submenu.remove(begin);
          if (item != null) {
            newMenu.add(item);
          } else {
            addSeparator(newMenu);
          }
        }
        menuList.add(newMenu);
      }
      for (int i = 0; i < menuList.size(); i++) {
        submenu.add(menuList.get(i), begin + i);
      }
      addSubmenu(menu, submenu, begin, end);
    } else {
      menu.add(submenu);
    }
  }

  /**
   * Update menuItem style depending on the page attributes.
   * 
   * @param menuItem Menu item.
   * @param page Page.
   */
  private static void updateFont(JMenuItem menuItem, Page page) {
    if ((menuItem == null) || (page == null)) {
      return;
    }
    if (page.isRedirect()) {
      menuItem.setFont(menuItem.getFont().deriveFont(Font.ITALIC));
    }
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      menuItem.setFont(menuItem.getFont().deriveFont(disambiguationAttributes));
    }
    if (Boolean.FALSE.equals(page.isExisting())) {
      menuItem.setFont(menuItem.getFont().deriveFont(missingAttributes));
    }
  }

  /**
   * Add a separator to a menu if useful.
   * 
   * @param menu Menu.
   * @return Number of items added.
   */
  private static int addSeparator(JMenu menu) {
    if ((menu == null) || (menu.getMenuComponentCount() == 0)) {
      return 0;
    }
    Component item = menu.getMenuComponent(menu.getMenuComponentCount() - 1);
    if (!(item instanceof JMenuItem)) {
      return 0;
    }
    menu.add(new JSeparator());
    return 1;
  }
}
