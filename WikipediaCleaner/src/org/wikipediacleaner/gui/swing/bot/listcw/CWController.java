package org.wikipediacleaner.gui.swing.bot.listcw;

import org.wikipediacleaner.api.MediaWikiController;
import org.wikipediacleaner.api.MediaWikiListener;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * Controller for background tasks.
 */
class CWController extends MediaWikiController {

  /**
   * @param listener Listener to MediaWiki events.
   */
  public CWController(MediaWikiListener listener) {
    super(listener);
  }

  /**
   * @param task Task to be performed in background.
   * @see MediaWikiController#addTask(java.util.concurrent.Callable)
   */
  @Override
  public void addTask(Callable<?> task) {
    // hasFinished(); // To clean up done tasks
    while (getRemainingTasksCount() > 10000) {
      try {
        // logCW.info("Too many tasks remaining, waiting a bit");
        TimeUnit.MILLISECONDS.sleep(1000);
      } catch (InterruptedException e) {
        // Do nothing
      }
      cleanUpDone();
    }
    super.addTask(task);
    cleanUpDone();
  }

  /**
   * Clean up tasks that are done and finished.
   */
  private void cleanUpDone() {
    while (getFirstResultIfDone() != null) {
      // Do nothing, done result is simply removed from the list of tasks to clean up done tasks
    }
  }

  /**
   * @return True if all tasks are completed.
   */
  public boolean hasFinished() {
    while (hasRemainingTask()) {
      Future<?> result = getFirstResultIfDone();
      if (result == null) {
        return false;
      }
    }
    return !hasRemainingTask();
  }
}
