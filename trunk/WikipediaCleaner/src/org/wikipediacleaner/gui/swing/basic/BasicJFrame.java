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
