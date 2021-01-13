/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.menu;

import java.awt.Color;
import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.font.TextAttribute;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.gui.swing.basic.Utilities;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueInteger;


/**
 * A helper class to manage contextual menu.
 */
public abstract class AbstractMenuCreator {

  // ==========================================================================
  // Managing menu structure
  // ==========================================================================

  /**
   * Add a separator to a menu if useful.
   * 
   * @param menu Menu.
   * @return Number of items added.
   */
  public int addSeparator(JMenu menu) {
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

  /**
   * Add a separator to a menu if useful.
   * 
   * @param menu Menu.
   * @return Number of items added.
   */
  public int addSeparator(JPopupMenu menu) {
    if ((menu == null) || (menu.getComponentCount() == 0)) {
      return 0;
    }
    Component item = menu.getComponent(menu.getComponentCount() - 1);
    if (!(item instanceof JMenuItem)) {
      return 0;
    }
    menu.add(new JSeparator());
    return 1;
  }

  /**
   * Add a disabled text to a menu.
   * 
   * @param menu Menu.
   * @param text Text of the item.
   * @return Number of items added.
   */
  public int addDisabledText(JMenu menu, String text) {
    if ((menu == null) || (text == null) || (text.length() == 0)) {
      return 0;
    }
    JMenuItem menuItem = new JMenuItem(text);
    menuItem.setEnabled(false);
    menu.add(menuItem);
    return 1;
  }

  /**
   * Add a disabled text to a menu.
   * 
   * @param menu Menu.
   * @param text Text of the item.
   * @return Number of items added.
   */
  public int addDisabledText(JPopupMenu menu, String text) {
    if ((menu == null) || (text == null) || (text.length() == 0)) {
      return 0;
    }
    JMenuItem menuItem = new JMenuItem(text);
    menuItem.setEnabled(false);
    menu.add(menuItem);
    return 1;
  }

  /**
   * Add an item to a menu.
   * 
   * @param menu Menu.
   * @param page Page.
   * @param title Element title (if null, the page title will be used).
   * @param asIs True if the message should be used as is (no mnemonic).
   * @param action Action.
   * @return Number of items added.
   */
  public int addItem(
      JMenu menu, Page page, String title, boolean asIs,
      ActionListener action) {
    return addItem(menu, page, title, asIs, action, null);
  }

  /**
   * Add an item to a menu.
   * 
   * @param menu Menu.
   * @param page Page.
   * @param title Element title (if null, the page title will be used).
   * @param asIs True if the message should be used as is (no mnemonic).
   * @param action Action.
   * @param accelerator Accelerator.
   * @return Number of items added.
   */
  public int addItem(
      JMenu menu, Page page, String title, boolean asIs,
      ActionListener action, KeyStroke accelerator) {
    if (menu == null) {
      return 0;
    }
    if ((title == null) && ((page == null) || (page.getTitle() == null))) {
      return 0;
    }
    JMenuItem menuItem = Utilities.createJMenuItem(title != null ? title : page.getTitle(), asIs);
    if (page != null) {
      updateFont(menuItem, page);
    }
    if (action != null) {
      menuItem.addActionListener(action);
    }
    if (accelerator != null) {
      menuItem.setAccelerator(accelerator);
    }
    menu.add(menuItem);
    return 1;
  }

  /**
   * Add an item to a menu.
   * 
   * @param menu Menu.
   * @param page Page.
   * @param title Element title (if null, the page title will be used).
  * @param asIs True if the message should be used as is (no mnemonic).
   * @param action Action.
   * @return Number of items added.
   */
  public int addItem(
      JPopupMenu menu, Page page, String title, boolean asIs,
      ActionListener action) {
    return addItem(menu, page, title, asIs, action, null);
  }

  /**
   * Add an item to a menu.
   * 
   * @param menu Menu.
   * @param page Page.
   * @param title Element title (if null, the page title will be used).
   * @param asIs True if the message should be used as is (no mnemonic).
   * @param action Action.
   * @param accelerator Accelerator.
   * @return Number of items added.
   */
  public int addItem(
      JPopupMenu menu, Page page, String title, boolean asIs,
      ActionListener action, KeyStroke accelerator) {
    if (menu == null) {
      return 0;
    }
    if ((title == null) && ((page == null) || (page.getTitle() == null))) {
      return 0;
    }
    JMenuItem menuItem = Utilities.createJMenuItem(title != null ? title : page.getTitle(), asIs);
    if (page != null) {
      updateFont(menuItem, page);
    }
    if (action != null) {
      menuItem.addActionListener(action);
    }
    if (accelerator != null) {
      menuItem.setAccelerator(accelerator);
    }
    menu.add(menuItem);
    return 1;
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
  public void addSubmenu(JMenu menu, JMenu submenu, int begin, int end) {
    addSubmenu(menu.getPopupMenu(), submenu, begin, end);
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
  public void addSubmenu(JPopupMenu menu, JMenu submenu, int begin, int end) {
    Configuration config = Configuration.getConfiguration();
    final int maxElements = Math.max(
        config.getInt(null, ConfigurationValueInteger.MENU_SIZE),
        begin + end + 2);
    if (submenu.getMenuComponentCount() > maxElements) {
      List<JMenu> menuList = new ArrayList<>();
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
   * Add submenus to a menu.
   * 
   * @param menu Menu.
   * @param items Submenus.
   */
  public void addSubmenus(JPopupMenu menu, List<JMenuItem> items) {
    Configuration config = Configuration.getConfiguration();
    final int maxElements = Math.max(config.getInt(null, ConfigurationValueInteger.MENU_SIZE), 10);
    if (items.size() <= maxElements) {
      for (JMenuItem item : items) {
        menu.add(item);
      }
      return;
    }
    int i = 0;
    while (i < items.size()) {
      JMenu currentMenu = new JMenu(items.get(i).getText());
      menu.add(currentMenu);
      while ((i < items.size()) && (currentMenu.getItemCount() < maxElements)) {
        currentMenu.add(items.get(i));
        i++;
      }
    }
  }

  // ==========================================================================
  // Rendering menu items
  // ==========================================================================

  /**
   * Attributes for rendering menu items for disambiguation pages.
   */
  final private static Map<TextAttribute, Color> disambiguationAttributes = new HashMap<>();

  /**
   * Attributes for rendering menu items for missing pages.
   */
  final private static Map<TextAttribute, Boolean> missingAttributes = new HashMap<>();

  /**
   * Attributes for rendering menu items for redirect pages.
   */
  final private static Map<TextAttribute, Float> redirectAttributes = new HashMap<>();

  static {
    disambiguationAttributes.put(TextAttribute.FOREGROUND, Color.RED);
    missingAttributes.put(TextAttribute.STRIKETHROUGH, TextAttribute.STRIKETHROUGH_ON);
    redirectAttributes.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
  }

  /**
   * Update menuItem style depending on the page attributes.
   * 
   * @param menuItem Menu item.
   * @param page Page.
   */
  protected void updateFont(JMenuItem menuItem, Page page) {
    if ((menuItem == null) || (page == null)) {
      return;
    }
    if (page.getRedirects().isRedirect()) {
      menuItem.setFont(menuItem.getFont().deriveFont(redirectAttributes));
    }
    if (Boolean.TRUE.equals(page.isDisambiguationPage())) {
      menuItem.setFont(menuItem.getFont().deriveFont(disambiguationAttributes));
    }
    if (Boolean.FALSE.equals(page.isExisting())) {
      menuItem.setFont(menuItem.getFont().deriveFont(missingAttributes));
    }
  }
}
