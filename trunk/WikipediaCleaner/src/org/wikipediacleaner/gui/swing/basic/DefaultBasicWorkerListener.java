/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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


/**
 * 
 */
public class DefaultBasicWorkerListener implements BasicWorkerListener {

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void beforeStart(
      @SuppressWarnings("unused") BasicWorker worker) {
    //
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterStart(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void afterStart(
      @SuppressWarnings("unused") BasicWorker worker) {
    //
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#beforeFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker)
   */
  public void beforeFinished(
      @SuppressWarnings("unused") BasicWorker worker) {
    //
  }

  /* (non-Javadoc)
   * @see org.wikipediacleaner.gui.swing.basic.BasicWorkerListener#afterFinished(org.wikipediacleaner.gui.swing.basic.BasicWorker, boolean)
   */
  public void afterFinished(
      @SuppressWarnings("unused") BasicWorker worker,
      @SuppressWarnings("unused") boolean ok) {
    //
  }
}
