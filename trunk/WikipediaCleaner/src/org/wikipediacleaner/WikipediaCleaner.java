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
import javax.swing.UnsupportedLookAndFeelException;

import org.wikipediacleaner.gui.swing.MainWindow;
import org.wikipediacleaner.gui.swing.component.CheckThreadViolationRepaintManager;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * Wikipedia Cleaner.
 */
public class WikipediaCleaner {

  private final static boolean CHECK_EDT = false;

  /**
   * @param args
   */
  public static void main(String[] args) {
    // Log levels
    Logger.getLogger("org.lobobrowser").setLevel(Level.WARNING);

    if (CHECK_EDT) {
      RepaintManager.setCurrentManager(new CheckThreadViolationRepaintManager());
    }

    // User Interface
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
    } catch (ClassNotFoundException e) {
      // Not important
    } catch (InstantiationException e) {
      // Not important
    } catch (IllegalAccessException e) {
      // Not important
    } catch (UnsupportedLookAndFeelException e) {
      // Not important
    }

    // Language
    Configuration config = Configuration.getConfiguration();
    GT.setCurrentLanguage(config.getLanguage());

    // Running
    MainWindow.createMainWindow();
  }

}
