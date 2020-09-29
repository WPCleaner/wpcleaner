/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Field;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.wikipediacleaner.Version;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.dataaccess.PageProvider;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.images.EnumImageSize;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.StringChecker;


/**
 * A base class for all Wikipedia Cleaner windows.
 */
public abstract class BasicWindow implements ActionListener, PageProvider {

  /** Logger */
  final static Logger static_log = LoggerFactory.getLogger(BasicWindow.class);

  static private ImageIcon icon;
  private JFrame parentComponent;
  private ProgressPanel glassPane;
  private Logger log;
  private EnumWikipedia wikipedia;

  /**
   * A message to use for experimental features.
   */
  public final static String experimentalMessage =
      "This function is experimental. Use at your own risk.\nDo you want to proceed ?";

  static {
    icon = Utilities.getImageIcon("commons-nuvola-web-broom.png", EnumImageSize.VERY_BIG);
  }

  /**
   * Constructor.
   */
  protected BasicWindow() {
    this.glassPane = new ProgressPanel(GT._T("{0} is working...", Version.PROGRAM));
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
      @Override
      public void run() {
        try {
          internalCreateWindow(
              name, wikipedia,
              closeOperation, windowClass, creation);
        } catch (Throwable t) {
          static_log.error("Error displaying window " + name, t);
        }
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
    Logger log = LoggerFactory.getLogger(windowClass);
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
    if (icon != null) {
      frame.setIconImage(icon.getImage());
    }

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
   * @return Wiki.
   */
  @Override
  public EnumWikipedia getWiki() {
    return wikipedia;
  }

  /**
   * @param wikipedia Wikipedia.
   */
  protected void setWikipedia(EnumWikipedia wikipedia) {
    this.wikipedia = wikipedia;
  }

  /**
   * @return Page.
   */
  @Override
  public Page getPage() {
    return null;
  }

  /**
   * @return WPCleaner configuration.
   */
  public WPCConfiguration getConfiguration() {
    if (wikipedia != null) {
      return wikipedia.getConfiguration();
    }
    return null;
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
   * Enable or disable a component.
   * 
   * @param component Component.
   * @param enabled True to enable the component.
   */
  protected void setEnabledStatus(Component component, boolean enabled) {
    if (component != null) {
      component.setEnabled(enabled);
    }
  }

  /**
   * Make a component visible or invisible.
   * 
   * @param component Component.
   * @param enabled True to make the component visible.
   */
  protected void setVisibleStatus(Component component, boolean enabled) {
    if (component != null) {
      component.setVisible(enabled);
    }
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
  public JFrame getParentComponent() {
    return parentComponent;
  }

  /**
   * @param state State of the frame.
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
    // TODO: Let users copy / paste the URL
    displayInformationMessage(message + "\n" + url);
  }

  /**
   * Ask the user to input a value.
   * 
   * @param message Message.
   * @param value Default value.
   * @param checker String checker to verify the value.
   * @return Value provided by the user.
   */
  public String askForValue(String message, String value, StringChecker checker) {
    return Utilities.askForValue(parentComponent, message, value, checker);
  }

  /**
   * @return Logger.
   */
  protected Logger getLog() {
    return log;
  }

  /**
   * Log an error message.
   * 
   * @param message Message.
   * @param error Error.
   */
  protected void logError(Object message, Throwable error) {
    log.error(message != null ? message.toString() : "", error);
  }

  /* ====================================================================== */
  /* ActionListener                                                         */
  /* ====================================================================== */

  /**
   * Invoked when an action occurs.
   * 
   * @param e Event.
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    //
  }
}
