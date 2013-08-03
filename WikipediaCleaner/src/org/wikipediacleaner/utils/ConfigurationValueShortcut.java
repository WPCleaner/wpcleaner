/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.awt.event.InputEvent;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;


/**
 * Configuration for Shortcut attributes.
 */
public enum ConfigurationValueShortcut {

  ADD_TO_WATCH_LIST("AddWatch", new ShortcutProperties(true, false, false, 'W')),
  APPLY("Apply", new ShortcutProperties(true, false, false, 'A')),
  CLOSE("Close", new ShortcutProperties(true, false, false, 'C')),
  DAB_ANALYSIS("Disambiguation", new ShortcutProperties(true, false, false, 'D')),
  EXTERNAL_VIEWER("ExternalViewer", new ShortcutProperties(true, false, false, 'E')),
  FULL_ANALYSIS("Title", new ShortcutProperties(true, false, false, 'F')),
  HISTORY("History", new ShortcutProperties(true, false, false, 'H')),
  RESTORE_DEFAULTS("RestoreDefaults", new ShortcutProperties(true, false, false, 'R')),
  VALIDATE("Validate", new ShortcutProperties(true, false, false, 'V'));

  private final static String PROPERTY_ENABLED = "Enabled";
  private final static String PROPERTY_CTRL = "Ctrl";
  private final static String PROPERTY_ALT = "Alt";
  private final static String PROPERTY_KEY = "KEY";

  /**
   * Attribute name.
   */
  private final String name;

  /**
   * Attribute default value.
   */
  private final ShortcutProperties defaultValue;

  /**
   * @param name Attribute name.
   * @param defaultValue Attribute default value.
   */
  ConfigurationValueShortcut(String name, ShortcutProperties defaultValue) {
    this.name = name;
    this.defaultValue = defaultValue;
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
    int key = preferences.getInt(
        PROPERTY_KEY, defaultProperties.getKey());
    return new ShortcutProperties(enabled, ctrl, alt, key);
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
    return name;
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

    private final boolean ctrl;
    private final boolean alt;

    private final int key;

    /**
     * @param enabled Is shortcut enabled ?
     * @param ctrl True if key Ctrl should be used.
     * @param alt True if key Alt should be used.
     * @param key Key.
     */
    public ShortcutProperties(
        boolean enabled,
        boolean ctrl, boolean alt,
        int key) {
      this.enabled = enabled;
      this.ctrl = ctrl;
      this.alt = alt;
      this.key = key;
    }

    /**
     * @return Is shortcut enabled ?
     */
    public boolean getEnabled() {
      return enabled;
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
     * @return Key.
     */
    public int getKey() {
      return key;
    }

    /**
     * @return True if mnemonic should be used.
     */
    public boolean useMnemonic() {
      return !ctrl && !alt;
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
      return modifiers;
    }

    /**
     * @return Textual description of the shortcut.
     */
    public String getDescription() {
      if (key == 0) {
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
          buffer.append("Atl + ");
        }
      }
      buffer.append(key);
      buffer.append(")");
      return buffer.toString();
    }
  }
}
