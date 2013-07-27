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
   * @throws HeadlessException
   */
  public BasicJFrame() throws HeadlessException {
    super();
  }

  /**
   * @param gc
   */
  public BasicJFrame(GraphicsConfiguration gc) {
    super(gc);
  }

  /**
   * @param title
   * @param gc
   */
  public BasicJFrame(String title, GraphicsConfiguration gc) {
    super(title, gc);
  }

  /**
   * @param title
   * @throws HeadlessException
   */
  public BasicJFrame(String title) throws HeadlessException {
    super(title);
  }

  /**
   * @return Window version.
   */
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
