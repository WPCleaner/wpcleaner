/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.gui.swing.basic;

import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;

import javax.swing.JFrame;

import org.wikipediacleaner.utils.Versionned;


/**
 * An extension of JFrame for Wikipedia Cleaner windows
 */
public class BasicJFrame extends JFrame implements Versionned {

  private static final long serialVersionUID = -6326849068169650289L;
  private Integer version;

  /**
   * @throws HeadlessException Exception thrown in case of missing display.
   */
  public BasicJFrame() throws HeadlessException {
    super();
  }

  /**
   * @param gc Graphics configuration.
   */
  public BasicJFrame(GraphicsConfiguration gc) {
    super(gc);
  }

  /**
   * @param title JFrame title.
   * @param gc Graphics configuration.
   */
  public BasicJFrame(String title, GraphicsConfiguration gc) {
    super(title, gc);
  }

  /**
   * @param title JFrame title.
   * @throws HeadlessException Exception thrown in case of missing display.
   */
  public BasicJFrame(String title) throws HeadlessException {
    super(title);
  }

  /**
   * @return Window version.
   */
  @Override
  public Integer getVersion() {
    return version;
  }

  /**
   * @param version Window version.
   */
  void setVersion(Integer version) {
    this.version = version;
  }
}
