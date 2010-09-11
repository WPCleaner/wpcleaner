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

package org.wikipediacleaner.gui.swing.basic;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;


/**
 * A base class for all Wikipedia Cleaner windows.
 */
public abstract class BasicWindow implements ActionListener {

  private JFrame parentComponent;
  private ProgressPanel glassPane;
  private Log log;
  private EnumWikipedia wikipedia;

  /**
   * Constructor.
   */
  protected BasicWindow() {
    this.glassPane = new ProgressPanel(GT._("Wikipedia Cleaner is working..."));
  }

  /**
   * Create a basic window.
   * 
   * @param name Window name.
   * @param wikipedia Wikipedia.
   * @param closeOperation Sets the operation that will happen by default
   *                       when the user initiates a "close" on this frame.
   *                       {@link JFrame#setDefaultCloseOperation(int)}
   * @param windowClass Class of the window.
   * @param creation Listener of creation events.
   */
  protected static void createWindow(
      final String              name,
      final EnumWikipedia       wikipedia,
      final int                 closeOperation,
      final Class               windowClass,
      final BasicWindowListener creation) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        internalCreateWindow(
            name, wikipedia,
            closeOperation, windowClass, creation);
      }
    });
  }

  /**
   * Create a basic window.
   * 
   * @param name Window name.
   * @param wikipedia Wikipedia.
   * @param closeOperation Sets the operation that will happen by default
   *                       when the user initiates a "close" on this frame.
   *                       {@link JFrame#setDefaultCloseOperation(int)}
   * @param windowClass Class of the window.
   * @param creation Listener of creation events.
   */
  static void internalCreateWindow(
      String              name,
      EnumWikipedia       wikipedia,
      int                 closeOperation,
      Class               windowClass,
      BasicWindowListener creation) {
    // Window decorations
    JFrame.setDefaultLookAndFeelDecorated(true);

    // Window creation
    BasicWindow window;
    Log log = LogFactory.getLog(windowClass);
    try {
      window = (BasicWindow) windowClass.newInstance();
    } catch (InstantiationException e) {
      log.error("Error creating window: " + e.getMessage());
      return;
    } catch (IllegalAccessException e) {
      log.error("Error creating window: " + e.getMessage());
      return;
    } catch (ClassCastException e) {
      return;
    }
    window.log = log;
    window.wikipedia = wikipedia;
    if (creation != null) {
      creation.initializeWindow(window);
    }

    // Frame creation
    BasicJFrame frame = new BasicJFrame(window.getTitle());
    frame.setName(name);
    frame.setDefaultCloseOperation(closeOperation);
    window.setParentComponent(frame);
    frame.setVersion(window.getVersion());

    // Menu Bar creation
    JMenuBar menuBar = window.createMenuBar();
    if (menuBar != null) {
      frame.setJMenuBar(menuBar);
    }

    // Window components
    Component components = window.createComponents();
    frame.getContentPane().add(components, BorderLayout.CENTER);

    // Display window
    frame.pack();
    Configuration config = Configuration.getConfiguration();
    config.restoreWindowPosition(frame);
    frame.setVisible(true);
    if (creation != null) {
      creation.displayWindow(window);
    }
  }

  /**
   * @return Window title.
   */
  public String getTitle() {
    return "Wikipedia Cleaner";
  }

  /**
   * @param title Window title.
   */
  public void setWindowTitle(String title) {
    if (parentComponent != null) {
      parentComponent.setTitle(title);
    }
  }

  /**
   * @return Wikipedia.
   */
  public EnumWikipedia getWikipedia() {
    return wikipedia;
  }

  /**
   * @param wikipedia Wikipedia.
   */
  protected void setWikipedia(EnumWikipedia wikipedia) {
    this.wikipedia = wikipedia;
  }

  /**
   * @return Window version if available.
   */
  public Integer getVersion() {
    try {
      Field field = getClass().getField("WINDOW_VERSION");
      if (field != null) {
        Object object = field.get(null);
        if (object instanceof Integer) {
          return (Integer) object;
        }
      }
    } catch (IllegalAccessException e) {
      logError("Error trying to retrieve field WINDOW_VERSION", e);
    } catch (IllegalArgumentException e) {
      logError("Error trying to retrieve field WINDOW_VERSION", e);
    } catch (NoSuchFieldException e) {
      // Normal : the field is optional
    } catch (NullPointerException e) {
      logError("Error trying to retrieve field WINDOW_VERSION", e);
    } catch (SecurityException e) {
      logError("Error trying to retrieve field WINDOW_VERSION", e);
    }
    return null;
  }

  /**
   * Create menu bar.
   * 
   * @return Menu bar.
   */
  protected JMenuBar createMenuBar() {
    return null;
  }

  /**
   * Create components composing the window.
   * 
   * @return Components.
   */
  protected Component createComponents() {
    return null;
  }

  /**
   * Update components states.
   */
  protected void updateComponentState() {
    //
  }

  /**
   * @return Glass pane.
   */
  public ProgressPanel getGlassPane() {
    return glassPane;
  }

  /**
   * @param parent Parent window.
   */
  private void setParentComponent(JFrame parent) {
    this.parentComponent = parent;
    parent.setGlassPane(glassPane);
  }

  /**
   * @return Parent window.
   */
  protected JFrame getParentComponent() {
    return parentComponent;
  }

  /**
   * @param state
   * @see Frame#setExtendedState(int)
   */
  public void setExtendedState(int state) {
    if (parentComponent != null) {
      parentComponent.setExtendedState(state);
    }
  }

  /**
   * Dispose window. 
   */
  public void dispose() {
    if (parentComponent != null) {
      parentComponent.dispose();
    }
  }

  /**
   * Display an error message.
   * 
   * @param exception Exception.
   */
  protected void displayError(Throwable exception) {
    Utilities.displayError(parentComponent, exception);
  }

  /**
   * Display a warning message.
   * 
   * @param message Message.
   */
  public void displayWarning(String message) {
    Utilities.displayWarning(parentComponent, message);
  }

  /**
   * Display a warning message.
   * 
   * @param message Message.
   * @param focus Component to give focus to.
   */
  protected void displayWarning(String message, Component focus) {
    Utilities.displayWarning(parentComponent, message, focus);
  }

  /**
   * Display a question with Yes/No answers.
   * 
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION}.
   */
  public int displayYesNoWarning(String message) {
    return Utilities.displayYesNoWarning(parentComponent, message);
  }

  /**
   * Display a question with Yes/Yes all/No/No all answers.
   * 
   * @param message Message.
   * @return Answer {@link JOptionPane#YES_OPTION} or {@link JOptionPane#NO_OPTION}.
   */
  public int displayYesNoAllWarning(String message) {
    return Utilities.displayYesNoAllWarning(parentComponent, message);
  }

  /**
   * Display an information message.
   * 
   * @param message Message.
   */
  protected void displayInformationMessage(String message) {
    Utilities.displayInformationMessage(parentComponent, message);
  }

  /**
   * Display an URL message.
   * 
   * @param message Message.
   * @param url URL.
   */
  protected void displayUrlMessage(String message, String url) {
    // TODO: Let users copy / paste the url
    displayInformationMessage(message + "\n" + url);
  }

  /**
   * Ask the user to input a value.
   * 
   * @param message Message.
   * @param value Default value.
   * @param unauthorizedCharacters Unauthorized characters.
   * @return Value provided by the user.
   */
  protected String askForValue(String message, String value, String unauthorizedCharacters) {
    return Utilities.askForValue(parentComponent, message, value, unauthorizedCharacters);
  }

  /**
   * @return Logger.
   */
  protected Log getLog() {
    return log;
  }

  /**
   * Log an error message.
   * 
   * @param message Message.
   * @param error Error.
   */
  protected void logError(Object message, Throwable error) {
    log.error(message, error);
  }

  /* ====================================================================== */
  /* ActionListener                                                         */
  /* ====================================================================== */

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  public void actionPerformed(ActionEvent e) {
    //
  }
}
