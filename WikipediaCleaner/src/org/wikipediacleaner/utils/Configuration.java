/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.utils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javax.swing.JOptionPane;

import org.wikipediacleaner.WikipediaCleaner;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.impl.MediaWikiAPI;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.ConfigurationValueStyle.StyleProperties;


/**
 * Configuration.
 */
public class Configuration implements WindowListener {

  private static Configuration configuration;

  // Properties
  public  final static String  PROPERTIES_LAST_REPLACEMENT = "LastReplacement";

  // Array properties
  public  final static String  ARRAY_CHECK_SELECTION     = "CheckWikiSelection";
  public  final static String  ARRAY_CHECK_BOT_SELECTION = "CheckBotSelection";
  public  final static String  ARRAY_FILTER_NS           = "FilterNS";
  public  final static String  ARRAY_INTERESTING_PAGES   = "InterestingPages";
  public  final static String  ARRAY_SORT_ORDERS         = "SortOrders";
  public  final static String  ARRAY_SPELLING_INACTIVE   = "SpellingInactive";
  public  final static String  ARRAY_WATCH_PAGES         = "WatchPages";

  public  final static String  SUB_ARRAY_PREFERRED_DAB   = "PreferredDisambiguations";

  // Pojo properties
  public  final static String  POJO_AUTOMATIC_FIXING     = "AutomaticFixing";
  public  final static String  POJO_PAGE_COMMENTS        = "PageComments";
  public  final static String  POJO_STYLES               = "Styles";

  // Configuration version :
  //   1 : Initial version
  //   2 : Configuration for each wikipedia
  //   3 : Multiple users
  //   4 : Change default color for redirect links
  public  final static int     CURRENT_CONFIG_VERSION    = 4;

  // Properties
  public  final static String  PROPERTIES_BACKLINKS      = "Backlinks";
  public  final static String  PROPERTIES_USERS          = "Users";

  public  final static String  VALUE_PAGE_NORMAL         = "N";
  public  final static String  VALUE_PAGE_HELP_NEEDED    = "H";

  // Special properties
  private final static String  PROPERTY_LANGUAGE         = "Language";
  private final static String  PROPERTY_WIKIPEDIA        = "Wikipedia";
  private final static String  PROPERTY_WINDOW           = "Window";
  private final static String  PROPERTY_WINDOW_H         = "h";
  private final static String  PROPERTY_WINDOW_VERSION   = "version";
  private final static String  PROPERTY_WINDOW_W         = "w";
  private final static String  PROPERTY_WINDOW_X         = "x";
  private final static String  PROPERTY_WINDOW_Y         = "y";

  private final Preferences preferences;

  /**
   * @return Configuration singleton.
   */
  public static Configuration getConfiguration() {
    if (configuration == null) {
      configuration = new Configuration();
    }
    return configuration;
  }

  /**
   * Constructor.
   */
  private Configuration() {
    preferences = Preferences.userNodeForPackage(WikipediaCleaner.class);
  }

  /**
   * @return Preferences.
   */
  private Preferences getPreferences() {
    return getPreferences(null);
  }

  /**
   * @param wikipedia Wikipedia.
   * @return Preferences.
   */
  private Preferences getPreferences(EnumWikipedia wikipedia) {
    if (wikipedia == null) {
      return preferences;
    }
    return preferences.node("Wikipedia/" + wikipedia.getSettings().getCode());
  }

  /**
   * Check configuration version.
   * 
   * @param parent Parent component.
   */
  public void checkVersion(Component parent) {
    try {
      if (getPreferences() != null) {
        String[] children = getPreferences().childrenNames();
        String[] keys = getPreferences().keys();

        // Check if first time
        if (((children == null) || (children.length == 0)) &&
            ((keys == null) || (keys.length == 0))) {
          setInt(
              null,
              ConfigurationValueInteger.CONFIG_VERSION,
              CURRENT_CONFIG_VERSION);
          return;
        }

        // Check if version is already up to date
        int version = getInt(null, ConfigurationValueInteger.CONFIG_VERSION);
        if (version == CURRENT_CONFIG_VERSION) {
          return;
        }
        if (version > CURRENT_CONFIG_VERSION) {
          setInt(
              null,
              ConfigurationValueInteger.CONFIG_VERSION,
              CURRENT_CONFIG_VERSION);
          return;
        }

        // Update from version 1 to 2 : Configuration for each wikipedia
        EnumWikipedia preferredWikipedia = null;
        boolean ok = true;
        if (version < 2) {
          preferredWikipedia = askForPreferredWikipedia(parent);
          if (preferredWikipedia == null) {
            return;
          }
          ok &= moveChild(getPreferences(), getPreferences(preferredWikipedia), ARRAY_WATCH_PAGES);
          ok &= moveChild(getPreferences(), getPreferences(preferredWikipedia), POJO_AUTOMATIC_FIXING);
          ok &= moveChild(getPreferences(), getPreferences(preferredWikipedia), POJO_PAGE_COMMENTS);
          ok &= moveChild(getPreferences(), getPreferences(preferredWikipedia), SUB_ARRAY_PREFERRED_DAB);
        }

        // Update from version 2 to 3 : Several users
        if (ok && (version < 3)) {
          ConfigurationValueString.setValue(
              getPreferences(),
              ConfigurationValueString.USER_NAME,
              null);
          ConfigurationValueString.setValue(
              getPreferences(),
              ConfigurationValueString.PASSWORD,
              null);
        }
        if (ok) {
          setInt(
              null,
              ConfigurationValueInteger.CONFIG_VERSION,
              CURRENT_CONFIG_VERSION);
        }

        // Update from version 3 to 4 : Default color for redirect links
        if (ok && (version < 4)) {
          StyleProperties style = getStyle(ConfigurationValueStyle.INTERNAL_LINK_REDIRECT);
          if (Color.CYAN.equals(style.getForegroundColor())) {
            StyleProperties newStyle = new StyleProperties(
                style.getEnabled(),
                style.getForeground(), Color.BLUE,
                style.getBackground(), style.getBackgroundColor(),
                style.getItalic(), style.getBold(),
                style.getUnderline(), style.getStrikeThrough());
            ConfigurationValueStyle.setValue(
                getPreferences(),
                ConfigurationValueStyle.INTERNAL_LINK_REDIRECT,
                newStyle);
          }
        }
      }
    } catch (BackingStoreException e) {
      //
    }
  }

  /**
   * Remove a node.
   * 
   * @param prefs Preferences.
   * @param property Node name.
   */
  private void removeNode(Preferences prefs, String property) {
    if (prefs != null) {
      try {
        if (prefs.nodeExists(property)) {
          Preferences node = prefs.node(property);
          node.removeNode();
        }
      } catch (BackingStoreException e) {
        //
      }
    }
  }

  /**
   * Move a child.
   * 
   * @param oldParent Old parent node.
   * @param newParent New parent node.
   * @param childName Child name.
   * @return True if the child has been completely moved.
   * @throws BackingStoreException
   */
  private boolean moveChild(
      Preferences oldParent, Preferences newParent, String childName)
      throws BackingStoreException {
    if ((oldParent == null) || (childName == null)) {
      return true;
    }
    if (!oldParent.nodeExists(childName)) {
      return true;
    }
    if (newParent == null) {
      return false;
    }
    Preferences oldChild = oldParent.node(childName);
    Preferences newChild = newParent.node(childName);

    // Move keys
    String[] keyNames = oldChild.keys();
    if (keyNames != null) {
      for (String keyName : keyNames) {
        String value = oldChild.get(keyName, null);
        if (value != null) {
          newChild.put(keyName, value);
        }
        oldChild.remove(keyName);
      }
    }

    // Move children
    String[] childNames2 = oldChild.childrenNames();
    if (childNames2 != null) {
      for (String childName2 : childNames2) {
        moveChild(oldChild, newChild, childName2);
      }
    }

    // Clean up
    boolean ok = false;
    newChild.flush();
    keyNames = oldChild.keys();
    childNames2 = oldChild.childrenNames();
    if (((keyNames == null) || (keyNames.length == 0)) ||
        ((childNames2 == null) || (childNames2.length == 0))) {
      oldChild.removeNode();
      ok = true;
    }
    oldChild.flush();
    return ok;
  }

  /**
   * Ask user for its preferred Wikipedia.
   * 
   * @param parent Parent component.
   * @return Preferred wikipedia.
   */
  private EnumWikipedia askForPreferredWikipedia(Component parent) {
    Object result = JOptionPane.showInputDialog(
        parent,
        GT._T(
            "Wikipedia Cleaner options have changed to allow different settings for each Wikipedia.\n" +
            "Previously, options were global for every Wikipedia.\n" +
            "The current options will be saved as the options for your preferred Wikipedia.\n" +
            "What is your preferred Wikipedia ?"),
        "Wikipedia Cleaner", JOptionPane.QUESTION_MESSAGE,
        null, EnumWikipedia.values(), getWikipedia());
    if ((result != null) && (result instanceof EnumWikipedia)) {
      return (EnumWikipedia) result;
    }
    return null;
  }

  /**
   * Write configuration.
   */
  public void save() {
    if (getPreferences() != null) {
      try {
        getPreferences().flush();
      } catch (BackingStoreException e) {
        // Nothing can be done
      }
    }
  }

  /**
   * Method to be called when the configuration has been updated. 
   */
  public void updateConfiguration() {
    MediaWikiAPI.updateConfiguration();
  }

  /** Forced values */
  private final Map<String, String> forcedValues = new HashMap<>();

  /**
   * Force a value (override the configured value).
   * 
   * @param identifier Identifier of the value.
   * @param value Value itself.
   */
  public void forceValue(String identifier, String value) {
    if (identifier != null) {
      forcedValues.put(identifier, value);
    }
  }

  // ==========================================================================
  // String management
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @return Property value.
   */
  public String getString(
      EnumWikipedia wikipedia, ConfigurationValueString property) {
    String forcedValue = forcedValues.get(property.getName());
    if (forcedValue != null) {
      return forcedValue;
    }
    return ConfigurationValueString.getValue(getPreferences(wikipedia), property);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param value Property value.
   */
  public void setString(
      EnumWikipedia wikipedia, ConfigurationValueString property, String value) {
    ConfigurationValueString.setValue(getPreferences(wikipedia), property, value);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param value Property value.
   */
  public void setString(
      EnumWikipedia wikipedia, ConfigurationValueString property, char[] value) {
    if (value != null) {
      setString(wikipedia, property, new String(value));
    } else {
      setString(wikipedia, property, (String) null);
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param subProperty Sub property name.
   * @param value Sub property value.
   */
  public void setSubString(
      EnumWikipedia wikipedia, String property, String subProperty, String value) {
    if (getPreferences(wikipedia) != null) {
      Preferences node = getPreferences(wikipedia).node(property);
      if (value != null) {
        node.put(subProperty, value);
      } else {
        node.remove(subProperty);
      }
    }
  }

  // ==========================================================================
  // Properties management
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @return Property value.
   */
  public Properties getProperties(
      EnumWikipedia wikipedia, String property) {
    Properties values = new Properties();
    try {
      if ((getPreferences(wikipedia) != null) &&
          (getPreferences(wikipedia).nodeExists(property))) {
        Preferences node = getPreferences(wikipedia).node(property);
        String[] children = node.keys();
        for (String child : children) {
          values.setProperty(child, node.get(child, ""));
        }
      }
    } catch (BackingStoreException e) {
      //
    }
    return values;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param subProperty Sub property name.
   * @return Property value.
   */
  public Properties getSubProperties(
      EnumWikipedia wikipedia, String property, String subProperty) {
    Properties values = new Properties();
    try {
      if ((getPreferences(wikipedia) != null) &&
          (getPreferences(wikipedia).nodeExists(property)) &&
          (getPreferences(wikipedia).nodeExists(property + "/" + subProperty))) {
        Preferences node = getPreferences(wikipedia).node(property + "/" + subProperty);
        String[] children = node.keys();
        for (String child : children) {
          values.setProperty(child, node.get(child, ""));
        }
      }
    } catch (BackingStoreException e) {
      //
    }
    return values;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param value Property value.
   */
  public void setProperties(
      EnumWikipedia wikipedia, String property, Properties value) {
    if (getPreferences(wikipedia) != null) {
      // First, remove the old properties
      removeNode(getPreferences(wikipedia), property);

      // Create the new ones
      Preferences node = getPreferences(wikipedia).node(property);
      if (value != null) {
        for (Entry<Object, Object> p : value.entrySet()) {
          node.put(p.getKey().toString(), p.getValue().toString());
        }
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param subProperty Sub property name.
   * @param value Property value.
   */
  public void setSubProperties(
      EnumWikipedia wikipedia,
      String property, String subProperty,
      Properties value) {
    try {
      if (getPreferences(wikipedia) != null) {
        // First, remove the old properties
        if (getPreferences(wikipedia).nodeExists(property)) {
          removeNode(getPreferences(wikipedia), property + "/" + subProperty);
        }
  
        // Create the new ones
        if (value != null) {
          Preferences node = getPreferences(wikipedia).node(property + "/" + subProperty);
          for (Entry<Object, Object> p : value.entrySet()) {
            String key = p.getKey().toString();
            String keyValue = p.getValue().toString();
            if ((key != null) && (key.length() <= Preferences.MAX_KEY_LENGTH) &&
                (keyValue != null) && (keyValue.length() <= Preferences.MAX_VALUE_LENGTH)) {
              node.put(key, keyValue);
            }
          }
        }
      }
    } catch (BackingStoreException e) {
      //
    }
  }

  // ==========================================================================
  // String list management
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @return List of property values.
   */
  public List<String> getStringList(
      EnumWikipedia wikipedia, String property) {
    List<String> result = new ArrayList<String>();
    if (getPreferences(wikipedia) != null) {
      try {
        Preferences node = getPreferences(wikipedia).node(property);
        String[] children = node.keys();
        for (int i = 0; i < children.length; i++) {
          result.add(node.get(children[i], ""));
        }
      } catch (BackingStoreException e) {
        //
      }
    }
    return result;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param values Property values.
   */
  public void setStringList(
      EnumWikipedia wikipedia,
      String property, List<String> values) {
    if (getPreferences(wikipedia) != null) {
      // First, remove the old array list
      removeNode(getPreferences(wikipedia), property);

      // Create the new one
      Preferences node = getPreferences(wikipedia).node(property);
      if (values != null) {
        for (int i = 0; i < values.size(); i++) {
          node.put(Integer.toString(i), values.get(i));
        }
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param subProperty Sub-property name.
   * @return List of property values.
   */
  public List<String> getStringSubList(
      EnumWikipedia wikipedia, String property, String subProperty) {
    List<String> result = new ArrayList<String>();
    if (getPreferences(wikipedia) != null) {
      try {
        if (!getPreferences(wikipedia).nodeExists(property)) {
          return result;
        }
        String nodeName = property + "/" + subProperty;
        if (!getPreferences(wikipedia).nodeExists(nodeName)) {
          return result;
        }
        Preferences node = getPreferences(wikipedia).node(nodeName);
        String[] children = node.keys();
        for (int i = 0; i < children.length; i++) {
          result.add(node.get(children[i], ""));
        }
      } catch (BackingStoreException e) {
        //
      }
    }
    return result;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param subProperty Sub-property name.
   * @param values Property values.
   */
  public void setStringSubList(
      EnumWikipedia wikipedia,
      String property, String subProperty,
      List<String> values) {
    if (getPreferences(wikipedia) != null) {
      String nodeName = property + "/" + subProperty;

      // First, remove the old array list
      removeNode(getPreferences(wikipedia), nodeName);

      // Create the new one
      Preferences node = getPreferences(wikipedia).node(nodeName);
      if (values != null) {
        for (int i = 0; i < values.size(); i++) {
          node.put(Integer.toString(i), values.get(i));
        }
      }
    }
  }

  // ==========================================================================
  // POJO management (Plain Old Java Object)
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param values Property values.
   * @param idName Name of the field used as an Id.
   */
  public void setPojoMap(
      EnumWikipedia wikipedia,
      String property, Map<String, Object> values, String idName) {
    if (getPreferences(wikipedia) != null) {
      // First, remove the old array list
      removeNode(getPreferences(wikipedia), property);
      
      // Create the new one
      if (values != null) {
        for (Map.Entry<String, Object> entry : values.entrySet()) {
          addPojo(wikipedia, property, entry.getValue(), entry.getKey());
        }
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param name Pojo name.
   * @param valueClass Pojo class.
   * @return Pojo.
   */
  public Object getPojo(
      EnumWikipedia wikipedia, String property, String name, Class valueClass) {
    try {
      if ((getPreferences(wikipedia) != null) &&
          (property != null) &&
          (name != null) &&
          (valueClass != null)) {
        if (!getPreferences(wikipedia).nodeExists(property)) {
          return null;
        }
        Preferences globalNode = getPreferences(wikipedia).node(property);
        if (!globalNode.nodeExists(name)) {
          return null;
        }
        Preferences node = globalNode.node(name);
        Object result = valueClass.newInstance();
        Method[] methods = valueClass.getMethods();
        Method fixValuesMethod = null;
        for (Method m : methods) {
          if (Modifier.isPublic(m.getModifiers()) &&
              (m.getName().equals("fixValues")) &&
              (m.getGenericParameterTypes().length == 0)) {
            fixValuesMethod = m;
          }
          if (Modifier.isPublic(m.getModifiers()) &&
              m.getName().startsWith("set") &&
              (m.getGenericParameterTypes().length == 1)) {
            String parameterName = "" + Character.toLowerCase(m.getName().charAt(3)) + m.getName().substring(4);
            boolean exist = false;
            for (String key : node.keys()) {
              if (parameterName.equals(key)) {
                exist = true;
              }
            }
            if (exist) {
              Class parameterType = m.getParameterTypes()[0];
              if (String.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.get(parameterName, null));
              } else if (Integer.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.getInt(parameterName, 0));
              } else if (Boolean.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.getBoolean(parameterName, true));
              } else if (Double.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.getDouble(parameterName, 0));
              } else if (Long.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.getLong(parameterName, 0));
              } else if (Float.class.isAssignableFrom(parameterType)) {
                m.invoke(result, node.getFloat(parameterName, 0));
              } else if (Color.class.isAssignableFrom(parameterType)) {
                m.invoke(result, new Color(node.getInt(parameterName, 0)));
              }
            }
          }
        }
        if (fixValuesMethod != null) {
          fixValuesMethod.invoke(result);
        }
        return result;
      }
    } catch (BackingStoreException e) {
      //
    } catch (InstantiationException e) {
      //
    } catch (IllegalAccessException e) {
      //
    } catch (InvocationTargetException e) {
      //
    } catch (IllegalArgumentException e) {
      // Happens with names ending with a slash
    }
    return null;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param id Id of the value.
   */
  public void removePojo(
      EnumWikipedia wikipedia, String property, String id) {
    if ((getPreferences(wikipedia) != null) && (property != null) && (id != null)) {
      Preferences globalNode = getPreferences(wikipedia).node(property);
      try {
        if (globalNode.nodeExists(id)) {
          Preferences node = globalNode.node(id);
          node.removeNode();
        }
      } catch (BackingStoreException e) {
        //
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param value Property value.
   * @param id Id of the value.
   */
  public void addPojo(
      EnumWikipedia wikipedia, String property, Object value, String id) {
    if ((getPreferences(wikipedia) != null) &&
        (property != null) &&
        (value != null) &&
        (id != null)) {
      try {
        // Remove the old object
        Preferences globalNode = getPreferences(wikipedia).node(property);
        removeNode(globalNode, id);
        
        // Add the new object
        Method[] methods = value.getClass().getMethods();
        Preferences node = globalNode.node(id);
        for (Method m : methods) {
          if (Modifier.isPublic(m.getModifiers()) &&
              m.getName().startsWith("get") &&
              (m.getGenericParameterTypes().length == 0)) {
            String attributeName = "" + Character.toLowerCase(m.getName().charAt(3)) + m.getName().substring(4);
            Class returnType = m.getReturnType();
            Object attrib = m.invoke(value, (Object[]) null);
            if (attrib == null) {
              node.remove(attributeName);
            } else if (String.class.isAssignableFrom(returnType)) {
              node.put(attributeName, (String) attrib);
            } else if (Integer.class.isAssignableFrom(returnType)) {
              node.putInt(attributeName, (Integer) attrib);
            } else if (Boolean.class.isAssignableFrom(returnType)) {
              node.putBoolean(attributeName, (Boolean) attrib);
            } else if (Double.class.isAssignableFrom(returnType)) {
              node.putDouble(attributeName, (Double) attrib);
            } else if (Long.class.isAssignableFrom(returnType)) {
              node.putLong(attributeName, (Long) attrib);
            } else if (Float.class.isAssignableFrom(returnType)) {
              node.putFloat(attributeName, (Float) attrib);
            } else if (Color.class.isAssignableFrom(returnType)) {
              node.putInt(attributeName, ((Color) attrib).getRGB());
            }
          }
        }
      } catch (IllegalAccessException e) {
        //
      } catch (InvocationTargetException e) {
        //
      } catch (ClassCastException e) {
        //
      }
    }
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param name Pojo name.
   * @param valueClass Pojo class.
   * @return Pojo.
   */
  public Object[] getPojoArray(
      EnumWikipedia wikipedia, String property, String name, Class valueClass) {
    try {
      if ((getPreferences(wikipedia) != null) &&
          (property != null) &&
          (name != null) &&
          (valueClass != null)) {
        if (!getPreferences(wikipedia).nodeExists(property)) {
          return null;
        }
        Preferences globalNode = getPreferences(wikipedia).node(property);
        if (!globalNode.nodeExists(name)) {
          return null;
        }
        Preferences pageNode = globalNode.node(name);
        ArrayList<Object> results = new ArrayList<Object>();
        int i = 0;
        while (pageNode.nodeExists(Integer.toString(i))) {
          Preferences node = pageNode.node(Integer.toString(i));
          Object result = valueClass.newInstance();
          Method[] methods = valueClass.getMethods();
          Method fixValuesMethod = null;
          for (Method m : methods) {
            if (Modifier.isPublic(m.getModifiers()) &&
                (m.getName().equals("fixValues")) &&
                (m.getGenericParameterTypes().length == 0)) {
              fixValuesMethod = m;
            }
            if (Modifier.isPublic(m.getModifiers()) &&
                m.getName().startsWith("set") &&
                (m.getGenericParameterTypes().length == 1)) {
              String parameterName = "" + Character.toLowerCase(m.getName().charAt(3)) + m.getName().substring(4);
              boolean exist = false;
              for (String key : node.keys()) {
                if (parameterName.equals(key)) {
                  exist = true;
                }
              }
              if (exist) {
                Class parameterType = m.getParameterTypes()[0];
                if (String.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.get(parameterName, null));
                } else if (Integer.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.getInt(parameterName, 0));
                } else if (Boolean.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.getBoolean(parameterName, true));
                } else if (Double.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.getDouble(parameterName, 0));
                } else if (Long.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.getLong(parameterName, 0));
                } else if (Float.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, node.getFloat(parameterName, 0));
                } else if (Color.class.isAssignableFrom(parameterType)) {
                  m.invoke(result, new Color(node.getInt(parameterName, 0)));
                }
              }
            }
          }
          if (fixValuesMethod != null) {
            fixValuesMethod.invoke(result);
          }
          results.add(result);
          i++;
        }
        return results.toArray();
      }
    } catch (BackingStoreException e) {
      //
    } catch (InstantiationException e) {
      //
    } catch (IllegalAccessException e) {
      //
    } catch (InvocationTargetException e) {
      //
    } catch (IllegalArgumentException e) {
      // Happens with names ending with a slash
    }
    return null;
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property name.
   * @param values Property value.
   * @param id Id of the value.
   */
  public void addPojoArray(
      EnumWikipedia wikipedia, String property, Object[] values, String id) {
    if ((getPreferences(wikipedia) != null) &&
        (property != null) &&
        (values != null) &&
        (id != null)) {
      try {
        // Remove the old object
        Preferences globalNode = getPreferences(wikipedia).node(property);
        removeNode(globalNode, id);
        
        // Add the new objects
        Preferences pageNode = globalNode.node(id);
        for (int i = 0; i < values.length; i++) {
          Object value = values[i];
          Preferences node = pageNode.node(Integer.toString(i));
          Method[] methods = value.getClass().getMethods();
          for (Method m : methods) {
            if (Modifier.isPublic(m.getModifiers()) &&
                m.getName().startsWith("get") &&
                (m.getGenericParameterTypes().length == 0)) {
              String attributeName = "" + Character.toLowerCase(m.getName().charAt(3)) + m.getName().substring(4);
              Class returnType = m.getReturnType();
              Object attrib = m.invoke(value, (Object[]) null);
              if (attrib == null) {
                node.remove(attributeName);
              } else if (String.class.isAssignableFrom(returnType)) {
                node.put(attributeName, (String) attrib);
              } else if (Integer.class.isAssignableFrom(returnType)) {
                node.putInt(attributeName, (Integer) attrib);
              } else if (Boolean.class.isAssignableFrom(returnType)) {
                node.putBoolean(attributeName, (Boolean) attrib);
              } else if (Double.class.isAssignableFrom(returnType)) {
                node.putDouble(attributeName, (Double) attrib);
              } else if (Long.class.isAssignableFrom(returnType)) {
                node.putLong(attributeName, (Long) attrib);
              } else if (Float.class.isAssignableFrom(returnType)) {
                node.putFloat(attributeName, (Float) attrib);
              } else if (Color.class.isAssignableFrom(returnType)) {
                node.putInt(attributeName, ((Color) attrib).getRGB());
              }
            }
          }
        }
      } catch (IllegalAccessException e) {
        //
      } catch (InvocationTargetException e) {
        //
      } catch (ClassCastException e) {
        //
      }
    }
  }

  // ==========================================================================
  // Styles management
  // ==========================================================================

  /**
   * @param style Style.
   * @return Style value.
   */
  public ConfigurationValueStyle.StyleProperties getStyle(
      ConfigurationValueStyle style) {
    return ConfigurationValueStyle.getValue(getPreferences(), style);
  }

  /**
   * @param style Style.
   * @param value Style value.
   */
  public void setStyle(
      ConfigurationValueStyle style,
      ConfigurationValueStyle.StyleProperties value) {
    ConfigurationValueStyle.setValue(getPreferences(), style, value);
  }

  // ==========================================================================
  // Shortcut management
  // ==========================================================================

  /**
   * @param shortcut Shortcut.
   * @return Style value.
   */
  public ConfigurationValueShortcut.ShortcutProperties getShortcut(
      ConfigurationValueShortcut shortcut) {
    return ConfigurationValueShortcut.getValue(getPreferences(), shortcut);
  }

  /**
   * @param shortcut Shortcut.
   * @param value Shortcut value.
   */
  public void setShortcut(
      ConfigurationValueShortcut shortcut,
      ConfigurationValueShortcut.ShortcutProperties value) {
    ConfigurationValueShortcut.setValue(getPreferences(), shortcut, value);
  }

  // ==========================================================================
  // Integer management
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property.
   * @return Property value.
   */
  public int getInt(EnumWikipedia wikipedia, ConfigurationValueInteger property) {
    String forcedValue = forcedValues.get(property.getName());
    if (forcedValue != null) {
      try {
        return Integer.parseInt(forcedValue);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }
    return ConfigurationValueInteger.getValue(getPreferences(wikipedia), property);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property.
   * @param value Property value.
   */
  public void setInt(EnumWikipedia wikipedia, ConfigurationValueInteger property, int value) {
    ConfigurationValueInteger.setValue(getPreferences(wikipedia), property, value);
  }

  // ==========================================================================
  // Color management
  // ==========================================================================

  /**
   * @param property Property name.
   * @param defaultValue Default value.
   * @return Property value.
   */
  public Color getColor(String property, Color defaultValue) {
    if (getPreferences() != null) {
      return new Color(getPreferences().getInt(property, defaultValue.getRGB()));
    }
    return defaultValue;
  }

  /**
   * @param property Property name.
   * @param value Property value.
   */
  public void setColor(String property, Color value) {
    if (getPreferences() != null) {
      if (value != null) {
        getPreferences().putInt(property, value.getRGB());
      } else {
        getPreferences().remove(property);
      }
    }
  }

  // ==========================================================================
  // Boolean management
  // ==========================================================================

  /**
   * @param wikipedia Wikipedia.
   * @param property Property.
   * @return Property value.
   */
  public boolean getBoolean(EnumWikipedia wikipedia, ConfigurationValueBoolean property) {
    String forcedValue = forcedValues.get(property.getName());
    if (forcedValue != null) {
      try {
        return Boolean.parseBoolean(forcedValue);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }
    return ConfigurationValueBoolean.getValue(getPreferences(wikipedia), property);
  }

  /**
   * @param wikipedia Wikipedia.
   * @param property Property.
   * @param value Property value.
   */
  public void setBoolean(EnumWikipedia wikipedia, ConfigurationValueBoolean property, boolean value) {
    ConfigurationValueBoolean.setValue(getPreferences(wikipedia), property, value);
  }

  // ==========================================================================
  // Wikipedia and Language management
  // ==========================================================================

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    if (getPreferences() != null) {
      String wikipedia = getPreferences().get(PROPERTY_WIKIPEDIA, null);
      if (wikipedia != null) {
        return EnumWikipedia.getWikipedia(wikipedia);
      }
    }
    return EnumWikipedia.getDefaultWikipedia();
  }

  /**
   * @param wikipedia Wikipedia.
   */
  public void setWikipedia(EnumWikipedia wikipedia) {
    if ((getPreferences() != null) && (wikipedia != null)) {
      getPreferences().put(PROPERTY_WIKIPEDIA, wikipedia.getSettings().getCode());
    }
  }

  /**
   * @return Language.
   */
  public EnumLanguage getLanguage() {
    if (getPreferences() != null) {
      String language = getPreferences().get(PROPERTY_LANGUAGE, null);
      if (language != null) {
        return EnumLanguage.getLanguage(language);
      }
    }
    return EnumLanguage.getDefaultLanguage();
  }

  /**
   * @param language Language.
   */
  public void setLanguage(EnumLanguage language) {
    if ((getPreferences() != null) && (language != null)) {
      getPreferences().put(PROPERTY_LANGUAGE, language.getCode());
    }
  }

  // ==========================================================================
  // Window management
  // ==========================================================================

  /**
   * Move/Resize a window at the preferred position and size.
   * 
   * @param window The window.
   */
  public void restoreWindowPosition(Window window) {
    if (window == null) {
      return;
    }
    window.addWindowListener(this);
    if (getBoolean(null, ConfigurationValueBoolean.RESTORE_WINDOW) &&
        (getPreferences() != null)) {
      try {
        if (!getPreferences().nodeExists(PROPERTY_WINDOW)) {
          return;
        }
        Preferences node = getPreferences().node(PROPERTY_WINDOW);
        if (!node.nodeExists(window.getName())) {
          return;
        }
        node = node.node(window.getName());
        window.setLocation(
            node.getInt(PROPERTY_WINDOW_X, 0),
            node.getInt(PROPERTY_WINDOW_Y, 0));
        boolean restoreSize = true;
        if (window instanceof Versionned) {
          Integer version = ((Versionned) window).getVersion();
          if (version != null) {
            int storedVersion = node.getInt(PROPERTY_WINDOW_VERSION, 1);
            if (version.intValue() > storedVersion) {
              restoreSize = false;
            }
          }
        }
        if (restoreSize) {
          window.setSize(
              node.getInt(PROPERTY_WINDOW_W, 1000),
              node.getInt(PROPERTY_WINDOW_H, 700));
        }
      } catch (BackingStoreException e) {
        //
      }
    }
  }

  /**
   * Save a window position and size.
   * 
   * @param window The window.
   */
  public void saveWindowPosition(Window window) {
    if (getBoolean(null, ConfigurationValueBoolean.SAVE_WINDOW) &&
        (window != null) &&
        (getPreferences() != null)) {
      Preferences node = getPreferences().node(PROPERTY_WINDOW);
      node = node.node(window.getName());
      node.putInt(PROPERTY_WINDOW_X, window.getX());
      node.putInt(PROPERTY_WINDOW_Y, window.getY());
      node.putInt(PROPERTY_WINDOW_W, window.getWidth());
      node.putInt(PROPERTY_WINDOW_H, window.getHeight());
      Integer version = null;
      if (window instanceof Versionned) {
        version = ((Versionned) window).getVersion();
      }
      if (version != null) {
        node.putInt(PROPERTY_WINDOW_VERSION, version.intValue());
      } else {
        node.remove(PROPERTY_WINDOW_VERSION);
      }
    }
  }

  /* ================================= */
  /* = WindowListener implementation = */
  /* ================================= */

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowActivated(java.awt.event.WindowEvent)
   */
  @Override
  public void windowActivated(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
   */
  @Override
  public void windowClosed(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
   */
  @Override
  public void windowClosing(WindowEvent e) {
    if ((e != null) && (e.getWindow() != null)) {
      saveWindowPosition(e.getWindow());
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowDeactivated(java.awt.event.WindowEvent)
   */
  @Override
  public void windowDeactivated(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowDeiconified(java.awt.event.WindowEvent)
   */
  @Override
  public void windowDeiconified(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowIconified(java.awt.event.WindowEvent)
   */
  @Override
  public void windowIconified(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowOpened(java.awt.event.WindowEvent)
   */
  @Override
  public void windowOpened(@SuppressWarnings("unused") WindowEvent e) {
    //
  }
}
