/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;


/**
 * Configuration for Shortcut attributes.
 */
public enum ConfigurationValueShortcut {

  //                enabled  name                  CTRL   ALT    SHIFT  key
  ADD_TO_WATCH_LIST(  true,  "AddWatch",           false, true,  false, KeyEvent.VK_W),
  APPLY(              true,  "Apply",              false, true,  false, KeyEvent.VK_A),
  BUG_REPORT(         true,  "BugReport",          false, true,  false, KeyEvent.VK_I),
  CHECK_ARTICLE(      false, "CheckArticle",       false, true,  false, KeyEvent.VK_T),
  CLOSE(              true,  "Close",              false, true,  false, KeyEvent.VK_C),
  CURRENT_DAB_LIST(   true,  "CurrentDab",         false, true,  false, KeyEvent.VK_C),
  DAB_ANALYSIS(       true,  "Disambiguation",     false, true,  false, KeyEvent.VK_D),
  EXTERNAL_VIEWER(    true,  "ExternalViewer",     false, true,  false, KeyEvent.VK_E),
  FULL_ANALYSIS(      true,  "FullAnalysis",       false, true,  false, KeyEvent.VK_F),
  HELP(               true,  "Help",               false, false, false, KeyEvent.VK_F1),
  HISTORY(            true,  "History",            false, true,  false, KeyEvent.VK_H),
  LIST_ADD(           true,  "ListAdd",            true,  false, false, KeyEvent.VK_PLUS),
  LIST_REMOVE(        true,  "ListRemove",         true,  false, false, KeyEvent.VK_MINUS),
  LOGIN(              true,  "Login",              false, true,  false, KeyEvent.VK_L),
  LOGOUT(             false, "Logout",             false, true,  false, KeyEvent.VK_O),
  OCCURRENCE_FIRST(   true,  "FirstOccurrence",    false, true,  false, KeyEvent.VK_F),
  OCCURRENCE_LAST(    true,  "LastOccurrence",     false, true,  false, KeyEvent.VK_L),
  OCCURRENCE_NEXT(    true,  "NextOccurrence",     false, true,  false, KeyEvent.VK_N),
  OCCURRENCE_PREVIOUS(true,  "PreviousOccurrence", false, true,  false, KeyEvent.VK_P),
  OPTIONS(            true,  "Options",            false, true,  false, KeyEvent.VK_O),
  RANDOM_PAGE(        false, "RandomPage",         false, true,  false, KeyEvent.VK_R),
  RESTORE_DEFAULTS(   true,  "RestoreDefaults",    false, true,  false, KeyEvent.VK_R),
  SEND(               true,  "Send",               false, true,  false, KeyEvent.VK_S),
  SYSTEM_OPTIONS(     true,  "SystemOptions",      false, true,  false, KeyEvent.VK_Y),
  VALIDATE(           true,  "Validate",           false, true,  false, KeyEvent.VK_V),
  WATCH_LIST(         true,  "WatchList",          false, true,  false, KeyEvent.VK_W);

  private final static String PROPERTY_ENABLED = "Enabled";
  private final static String PROPERTY_CTRL    = "Ctrl";
  private final static String PROPERTY_ALT     = "Alt";
  private final static String PROPERTY_SHIFT   = "Shift";
  private final static String PROPERTY_KEY     = "Key";

  /**
   * Attribute default value.
   */
  private final ShortcutProperties defaultValue;

  /**
   * @param enabled Is shortcut enabled ?
   * @param name Shortcut name.
   * @param ctrl True if key Ctrl should be used.
   * @param alt True if key Alt should be used.
   * @param shift True if key Shift should be used.
   * @param key Key.
   */
  ConfigurationValueShortcut(
      boolean enabled, String name,
      boolean ctrl, boolean alt, boolean shift,
      int key) {
    this.defaultValue = new ShortcutProperties(enabled, name, ctrl, alt, shift, key);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @return Current value of the attribute.
   */
  static ShortcutProperties getValue(Preferences preferences, ConfigurationValueShortcut attribute) {
    if (attribute == null) {
      return null;
    }
    return attribute.getValue(preferences);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @return Current value of the attribute.
   */
  ShortcutProperties getValue(Preferences preferences) {
    ShortcutProperties defaultProperties = getDefaultValue();
    preferences = getShortcutNode(preferences, false);
    if (preferences == null) {
      return defaultProperties;
    }
    boolean enabled = preferences.getBoolean(
        PROPERTY_ENABLED,
        defaultProperties.getEnabled());
    boolean ctrl = preferences.getBoolean(
        PROPERTY_CTRL,
        defaultProperties.getCtrlKey());
    boolean alt = preferences.getBoolean(
        PROPERTY_ALT,
        defaultProperties.getAltKey());
    boolean shift = preferences.getBoolean(
        PROPERTY_SHIFT, defaultProperties.getShiftKey());
    int key = preferences.getInt(
        PROPERTY_KEY, defaultProperties.getKey());
    return new ShortcutProperties(
        enabled, defaultProperties.getName(), ctrl, alt, shift, key);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param attribute Attribute.
   * @param value New value of the attribute.
   */
  static void setValue(Preferences preferences, ConfigurationValueShortcut attribute, ShortcutProperties value) {
    if (attribute == null) {
      return;
    }
    attribute.setValue(preferences, value);
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param value New value of the attribute.
   */
  void setValue(Preferences preferences, ShortcutProperties value) {
    preferences = getShortcutNode(preferences, true);
    if (preferences == null) {
      return;
    }
    preferences.putBoolean(PROPERTY_ENABLED, value.getEnabled());
    preferences.putBoolean(PROPERTY_CTRL, value.getCtrlKey());
    preferences.putBoolean(PROPERTY_ALT, value.getAltKey());
    preferences.putBoolean(PROPERTY_SHIFT, value.getShiftKey());
    preferences.putInt(PROPERTY_KEY, value.getKey());
  }

  /**
   * @param preferences Root of preferences for WPCleaner.
   * @param create Flag indicating if the node should be created.
   * @return Node for the style.
   */
  private Preferences getShortcutNode(Preferences preferences, boolean create) {
    try {
      if (preferences == null) {
        return null;
      }
      if (!create && !preferences.nodeExists("Shortcuts")) {
        return null;
      }
      preferences = preferences.node("Shortcuts");
      if (!create && !preferences.nodeExists(getName())) {
        return null;
      }
      preferences = preferences.node(getName());
      return preferences;
    } catch (BackingStoreException e) {
      return null;
    }
  }

  /**
   * @return Name of the configuration attribute.
   */
  public String getName() {
    return defaultValue.getName();
  }

  /**
   * @return Default value of the attribute.
   */
  public ShortcutProperties getDefaultValue() {
    return defaultValue;
  }

  /**
   * Holder for Shortcut properties.
   */
  public static class ShortcutProperties {
    private final boolean enabled;
    private final String name;

    private final boolean ctrl;
    private final boolean alt;
    private final boolean shift;

    private final int key;

    /**
     * @param enabled Is shortcut enabled ?
     * @param name Shortcut name.
     * @param ctrl True if key Ctrl should be used.
     * @param alt True if key Alt should be used.
     * @param shift True if key Shift should be used.
     * @param key Key.
     */
    public ShortcutProperties(
        boolean enabled, String name,
        boolean ctrl, boolean alt, boolean shift,
        int key) {
      this.enabled = enabled;
      this.name = name;
      this.ctrl = ctrl;
      this.alt = alt;
      this.shift = shift;
      this.key = key;
    }

    /**
     * @return Is shortcut enabled ?
     */
    public boolean getEnabled() {
      return enabled;
    }

    /**
     * @return Shortcut name.
     */
    public String getName() {
      return name;
    }

    /**
     * @return True if key Ctrl should be used.
     */
    public boolean getCtrlKey() {
      return ctrl;
    }

    /**
     * @return True if key Alt should be used.
     */
    public boolean getAltKey() {
      return alt;
    }

    /**
     * @return True if key Shift should be used.
     */
    public boolean getShiftKey() {
      return shift;
    }

    /**
     * @return Key.
     */
    public int getKey() {
      return key;
    }

    /**
     * @return True if mnemonic should be used.
     */
    public boolean useMnemonic() {
      return false;
    }

    /**
     * @return Combination of InputEvent modifiers.
     */
    public int getModifiers() {
      int modifiers = 0;
      if (alt) {
        modifiers = modifiers | InputEvent.ALT_DOWN_MASK;
      }
      if (ctrl) {
        modifiers = modifiers | InputEvent.CTRL_DOWN_MASK;
      }
      if (shift) {
        modifiers = modifiers | InputEvent.SHIFT_DOWN_MASK;
      }
      return modifiers;
    }

    /**
     * @return Textual description of the shortcut.
     */
    public String getDescription() {
      if (key == -1) {
        return "";
      }
      StringBuilder buffer = new StringBuilder();
      buffer.append(" (");
      if (useMnemonic()) {
        buffer.append("Alt + ");
      } else {
        if (ctrl) {
          buffer.append("Ctrl + ");
        }
        if (alt) {
          buffer.append("Alt + ");
        }
        if (shift) {
          buffer.append("Shift + ");
        }
      }
      buffer.append(KeyEvent.getKeyText(key));
      buffer.append(")");
      return buffer.toString();
    }
  }
}
