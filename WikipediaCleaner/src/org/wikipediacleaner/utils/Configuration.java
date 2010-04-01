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

package org.wikipediacleaner.utils;

import java.awt.Window;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import org.wikipediacleaner.WikipediaCleaner;
import org.wikipediacleaner.api.constants.EnumLanguage;
import org.wikipediacleaner.api.constants.EnumWikipedia;


/**
 * Configuration.
 */
public class Configuration implements WindowListener {

  private static Configuration configuration;

  // String properties
  public  final static String  STRING_PAGE_NAME          = "PageName";
  public  final static String  STRING_PASSWORD           = "Password";
  public  final static String  STRING_SIGNATURE          = "Signature";
  public  final static String  STRING_USER_NAME          = "UserName";

  public  final static String  DEFAULT_SIGNATURE         = "--~~~~";

  // Array properties
  public  final static String  ARRAY_SORT_ORDERS         = "SortOrders";
  public  final static String  ARRAY_WATCH_PAGES         = "WatchPages";

  // Pojo properties
  public  final static String  POJO_AUTOMATIC_FIXING     = "AutomaticFixing";
  public  final static String  POJO_PAGE_COMMENTS        = "PageComments";

  // Integer properties
  public  final static String  INTEGER_ANALYSIS_NB_PAGES = "AnalysisNbPages";
  public  final static String  INTEGER_ANALYSIS_UNDO_LVL = "AnalysisUndoLevels";
  public  final static String  INTEGER_CHECK_NB_ERRORS   = "CheckNbErrors";
  public  final static String  INTEGER_INTERROG_THREAD   = "InterrogationThreads";
  public  final static String  INTEGER_MAXIMUM_PAGES     = "MaximumPages";
  public  final static String  INTEGER_MENU_SIZE         = "MenuSize";

  public  final static int     DEFAULT_ANALYSIS_NB_PAGES = 10;
  public  final static int     DEFAULT_ANALYSIS_UNDO_LVL = 10;
  public  final static int     DEFAULT_CHECK_NB_ERRORS   = 100;
  public  final static int     DEFAULT_INTERROG_THREAD   = 30;
  public  final static int     DEFAULT_MAXIMUM_PAGES     = 20;
  public  final static int     DEFAULT_MENU_SIZE         = 30;

  // Boolean properties
  public  final static String  BOOLEAN_ADVANCED_FEATURES       = "AdvancedFeatures";
  public  final static String  BOOLEAN_ANALYSIS_COUNT_DISAMBIG = "AnalysisCountDisambiguation";
  public  final static String  BOOLEAN_ANALYSIS_COUNT_MISSING  = "AnalysisCountMissing";
  public  final static String  BOOLEAN_ANALYSIS_COUNT_OTHER    = "AnalysisCountOther";
  public  final static String  BOOLEAN_ANALYSIS_COUNT_REDIRECT = "AnalysisCountRedirect";
  public  final static String  BOOLEAN_ANALYSIS_DISAMBIG_PAGES = "AnalysisDisambiguationPages";
  public  final static String  BOOLEAN_ANALYSIS_HIDE_SENDING   = "AnalysisHideSending";
  public  final static String  BOOLEAN_ANALYSIS_MISSING_PAGES  = "AnalysisMissingPages";
  public  final static String  BOOLEAN_ANALYSIS_OTHER_PAGES    = "AnalysisOtherPages";
  public  final static String  BOOLEAN_ANALYSIS_REDIRECT_PAGES = "AnalysisRedirectPages";
  public  final static String  BOOLEAN_CHECK_SHOW_0_ERRORS     = "CheckShow0Errors";
  public  final static String  BOOLEAN_CLOSE_DISAMBIG          = "CloseDisambiguation";
  public  final static String  BOOLEAN_CLOSE_FULL              = "CloseFullAnalysis";
  public  final static String  BOOLEAN_RESTORE_WINDOW          = "RestoreWindow";
  public  final static String  BOOLEAN_SAVE_WINDOW             = "SaveWindow";
  public  final static String  BOOLEAN_SHORT_NOTATION          = "ShortNotation";
  public  final static String  BOOLEAN_WIKICLEANER_COMMENT     = "WikiCleanerComment";

  public  final static boolean DEFAULT_ADVANCED_FEATURES       = false;
  public  final static boolean DEFAULT_ANALYSIS_COUNT_DISAMBIG = true;
  public  final static boolean DEFAULT_ANALYSIS_COUNT_MISSING  = false;
  public  final static boolean DEFAULT_ANALYSIS_COUNT_OTHER    = false;
  public  final static boolean DEFAULT_ANALYSIS_COUNT_REDIRECT = false;
  public  final static boolean DEFAULT_ANALYSIS_DISAMBIG_PAGES = true;
  public  final static boolean DEFAULT_ANALYSIS_HIDE_SENDING   = true;
  public  final static boolean DEFAULT_ANALYSIS_MISSING_PAGES  = false;
  public  final static boolean DEFAULT_ANALYSIS_OTHER_PAGES    = false;
  public  final static boolean DEFAULT_ANALYSIS_REDIRECT_PAGES = false;
  public  final static boolean DEFAULT_CHECK_SHOW_0_ERRORS     = false;
  public  final static boolean DEFAULT_CLOSE_DISAMBIG          = false;
  public  final static boolean DEFAULT_CLOSE_FULL              = true;
  public  final static boolean DEFAULT_RESTORE_WINDOW          = true;
  public  final static boolean DEFAULT_SAVE_WINDOW             = true;
  public  final static boolean DEFAULT_SHORT_NOTATION          = false;
  public  final static boolean DEFAULT_WIKICLEANER_COMMENT     = true;

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
   * Contructor.
   */
  private Configuration() {
    preferences = Preferences.userNodeForPackage(WikipediaCleaner.class);
  }

  /**
   * Write configuration.
   */
  public void save() {
    if (preferences != null) {
      try {
        preferences.flush();
      } catch (BackingStoreException e) {
        // Nothing can be done
      }
    }
  }

  /**
   * @param property Property name.
   * @return Property value.
   */
  public String getString(String property) {
    return getString(property, "");
  }

  /**
   * @param property Property name.
   * @param defaultValue Default value.
   * @return Property value.
   */
  public String getString(String property, String defaultValue) {
    if (preferences != null) {
      return preferences.get(property, defaultValue);
    }
    return defaultValue;
  }

  /**
   * @param property Property name.
   * @param value Property value.
   */
  public void setString(String property, String value) {
    if (preferences != null) {
      if (value != null) {
        preferences.put(property, value);
      } else {
        preferences.remove(property);
      }
    }
  }

  /**
   * @param property Property name.
   * @param value Property value.
   */
  public void setString(String property, char[] value) {
    if (value != null) {
      setString(property, new String(value));
    } else {
      preferences.remove(property);
    }
  }

  /**
   * @param property Property name.
   * @return Property value.
   */
  public Properties getProperties(String property) {
    Properties values = new Properties();
    if (preferences != null) {
      try {
        Preferences node = preferences.node(property);
        String[] children = node.keys();
        for (String child : children) {
          values.setProperty(child, node.get(child, ""));
        }
      } catch (BackingStoreException e) {
        //
      }
    }
    return values;
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
   * @param property Property name.
   * @param value Property value.
   */
  public void setProperties(String property, Properties value) {
    if (preferences != null) {
      // First, remove the old properties
      removeNode(preferences, property);

      // Create the new ones
      Preferences node = preferences.node(property);
      if (value != null) {
        for (Entry<Object, Object> p : value.entrySet()) {
          node.put(p.getKey().toString(), p.getValue().toString());
        }
      }
    }
  }

  /**
   * @param property Property name.
   * @return List of property values.
   */
  public ArrayList<String> getStringArrayList(String property) {
    ArrayList<String> result = new ArrayList<String>();
    if (preferences != null) {
      try {
        Preferences node = preferences.node(property);
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
   * @param property Property name.
   * @param values Property values.
   */
  public void setStringArrayList(String property, ArrayList<String> values) {
    if (preferences != null) {
      // First, remove the old array list
      removeNode(preferences, property);

      // Create the new one
      Preferences node = preferences.node(property);
      if (values != null) {
        for (int i = 0; i < values.size(); i++) {
          node.put(Integer.toString(i), values.get(i));
        }
      }
    }
  }

  /**
   * @param property Property name.
   * @param values Property values.
   * @param idName Name of the field used as an Id.
   */
  public void setPojoMap(String property, Map<String, Object> values, String idName) {
    if (preferences != null) {
      // First, remove the old array list
      removeNode(preferences, property);
      
      // Create the new one
      if (values != null) {
        for (Map.Entry<String, Object> entry : values.entrySet()) {
          addPojo(property, entry.getValue(), entry.getKey());
        }
      }
    }
  }

  /**
   * @param property Property name.
   * @param name Pojo name.
   * @param valueClass Pojo class.
   * @return Pojo.
   */
  public Object getPojo(String property, String name, Class valueClass) {
    try {
      if ((preferences != null) && (property != null) && (name != null) && (valueClass != null)) {
        if (!preferences.nodeExists(property)) {
          return null;
        }
        Preferences globalNode = preferences.node(property);
        if (!globalNode.nodeExists(name)) {
          return null;
        }
        Preferences node = globalNode.node(name);
        Object result = valueClass.newInstance();
        Method[] methods = valueClass.getMethods();
        for (Method m : methods) {
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
              }
            }
          }
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
   * @param property Property name.
   * @param id Id of the value.
   */
  public void removePojo(String property, String id) {
    if ((preferences != null) && (property != null) && (id != null)) {
      Preferences globalNode = preferences.node(property);
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
   * @param property Property name.
   * @param value Property value.
   * @param id Id of the value.
   */
  public void addPojo(String property, Object value, String id) {
    if ((preferences != null) && (property != null) && (value != null) && (id != null)) {
      try {
        // Remove the old object
        Preferences globalNode = preferences.node(property);
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
   * @param property Property name.
   * @param name Pojo name.
   * @param valueClass Pojo class.
   * @return Pojo.
   */
  public Object[] getPojoArray(String property, String name, Class valueClass) {
    try {
      if ((preferences != null) && (property != null) && (name != null) && (valueClass != null)) {
        if (!preferences.nodeExists(property)) {
          return null;
        }
        Preferences globalNode = preferences.node(property);
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
          for (Method m : methods) {
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
                }
              }
            }
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
   * @param property Property name.
   * @param values Property value.
   * @param id Id of the value.
   */
  public void addPojoArray(String property, Object[] values, String id) {
    if ((preferences != null) && (property != null) && (values != null) && (id != null)) {
      try {
        // Remove the old object
        Preferences globalNode = preferences.node(property);
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

  /**
   * @param property Property name.
   * @param defaultValue Default value.
   * @return Property value.
   */
  public int getInt(String property, int defaultValue) {
    if (preferences != null) {
      return preferences.getInt(property, defaultValue);
    }
    return defaultValue;
  }

  /**
   * @param property Property name.
   * @param value Property value.
   */
  public void setInt(String property, int value) {
    if (preferences != null) {
      preferences.putInt(property, value);
    }
  }

  /**
   * @param property Property name.
   * @param defaultValue Default value.
   * @return Property value.
   */
  public boolean getBoolean(String property, boolean defaultValue) {
    if (preferences != null) {
      return preferences.getBoolean(property, defaultValue);
    }
    return defaultValue;
  }

  /**
   * @param property Property name.
   * @param value Property value.
   */
  public void setBoolean(String property, boolean value) {
    if (preferences != null) {
      preferences.putBoolean(property, value);
    }
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    if (preferences != null) {
      String wikipedia = preferences.get(PROPERTY_WIKIPEDIA, null);
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
    if ((preferences != null) && (wikipedia != null)) {
      preferences.put(PROPERTY_WIKIPEDIA, wikipedia.getCode());
    }
  }

  /**
   * @return Language.
   */
  public EnumLanguage getLanguage() {
    if (preferences != null) {
      String language = preferences.get(PROPERTY_LANGUAGE, null);
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
    if ((preferences != null) && (language != null)) {
      preferences.put(PROPERTY_LANGUAGE, language.getCode());
    }
  }

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
    if (getBoolean(BOOLEAN_RESTORE_WINDOW, DEFAULT_RESTORE_WINDOW) &&
        (preferences != null)) {
      try {
        if (!preferences.nodeExists(PROPERTY_WINDOW)) {
          return;
        }
        Preferences node = preferences.node(PROPERTY_WINDOW);
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
    if (getBoolean(BOOLEAN_SAVE_WINDOW, DEFAULT_SAVE_WINDOW) &&
        (window != null) && (preferences != null)) {
      Preferences node = preferences.node(PROPERTY_WINDOW);
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
  public void windowActivated(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
   */
  public void windowClosed(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
   */
  public void windowClosing(WindowEvent e) {
    if ((e != null) && (e.getWindow() != null)) {
      saveWindowPosition(e.getWindow());
    }
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowDeactivated(java.awt.event.WindowEvent)
   */
  public void windowDeactivated(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowDeiconified(java.awt.event.WindowEvent)
   */
  public void windowDeiconified(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowIconified(java.awt.event.WindowEvent)
   */
  public void windowIconified(@SuppressWarnings("unused") WindowEvent e) {
    //
  }

  /* (non-Javadoc)
   * @see java.awt.event.WindowListener#windowOpened(java.awt.event.WindowEvent)
   */
  public void windowOpened(@SuppressWarnings("unused") WindowEvent e) {
    //
  }
}
