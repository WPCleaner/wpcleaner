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

package org.wikipediacleaner;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.RepaintManager;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import org.wikipediacleaner.gui.swing.MainWindow;
import org.wikipediacleaner.gui.swing.component.CheckThreadViolationRepaintManager;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationConstants;
import org.wikipediacleaner.utils.ConfigurationValueInteger;
import org.wikipediacleaner.utils.ConfigurationValueString;


/**
 * Wikipedia Cleaner.
 */
public class WikipediaCleaner {

  private final static boolean CHECK_EDT = false;
  private final static boolean SYSTEM_LF = true;

  /**
   * @param args
   */
  public static void main(String[] args) {
    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);
    Logger.getLogger("").setLevel(Level.WARNING);

    Configuration config = Configuration.getConfiguration();

    // Check that calls are made in the Event Dispatch Thread
    if (CHECK_EDT) {
      RepaintManager.setCurrentManager(new CheckThreadViolationRepaintManager());
    }

    // User Interface
    String lookAndFeelClassName = null;
    if (SYSTEM_LF) {
      lookAndFeelClassName = UIManager.getSystemLookAndFeelClassName();
    }
    switch (config.getInt(null, ConfigurationValueInteger.PLAF_TYPE)) {
    case ConfigurationConstants.VALUE_PLAF_TYPE_WPCLEANER:
      lookAndFeelClassName = getLookAndFeelClassName("Nimbus");
      break;
    case ConfigurationConstants.VALUE_PLAF_TYPE_USER:
      lookAndFeelClassName = getLookAndFeelClassName(
          config.getString(null, ConfigurationValueString.PLAF_NAME));
      break;
    }
    if (lookAndFeelClassName != null) {
      try {
        UIManager.setLookAndFeel(lookAndFeelClassName);
      } catch (ClassNotFoundException e) {
        // Not important
      } catch (InstantiationException e) {
        // Not important
      } catch (IllegalAccessException e) {
        // Not important
      } catch (UnsupportedLookAndFeelException e) {
        // Not important
      }
    }

    // Language
    GT.setCurrentLanguage(config.getLanguage());

    // Running
    MainWindow.createMainWindow();
  }

  /**
   * @param name Look and Feel name
   * @return Look and Feel class name
   */
  private static String getLookAndFeelClassName(String name) {
    if (name == null) {
      return null;
    }
    for (LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
      if (name.equals(info.getName())) {
        return info.getClassName();
      }
    }
    return null;
  }
}
