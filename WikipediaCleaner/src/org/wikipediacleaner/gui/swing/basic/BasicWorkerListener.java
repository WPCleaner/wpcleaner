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
 * An interface used to listen for BasicWorker events.
 */
public interface BasicWorkerListener {

  /**
   * Called just at the beginning of the start() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  public void beforeStart(BasicWorker worker);

  /**
   * Called just at the end of the start() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  public void afterStart(BasicWorker worker);

  /**
   * Called just at the beginning of the finished() method in BasicWorker.
   * 
   * @param worker Current worker.
   */
  public void beforeFinished(BasicWorker worker);
  
  /**
   * Called just at the end of the finished() method in BasicWorker.
   * 
   * @param worker Current worker.
   * @param ok Flag indicating if the worker finished OK.
   */
  public void afterFinished(BasicWorker worker, boolean ok);
}
