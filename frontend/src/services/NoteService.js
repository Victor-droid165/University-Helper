const apiNotesURL = process.env.REACT_APP_API + "/notes"
const getNoteIdRoute = apiNotesURL + "/getId"
const removeNoteRoute = apiNotesURL + "/removeNote"
const registerNoteRoute = apiNotesURL + "/registerNote"
const updateNoteRoute = apiNotesURL + "/updateANote"
const notifyUserRoute = apiNotesURL + "/notifyUser"

export default class NoteService {

  async getNoteId(prefix) {
    const response = await fetch(getNoteIdRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(prefix),
    });
    const data = await response.json();
    return data;
  }

  async removeNote(note) {
    await fetch(removeNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    })
  }

  async registerNote({ warnedUser, ...note }) {
    await fetch(registerNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    });
    if (note.noteType === 'Warning') {
      await this.warnUser({ dbWarningId: note.noteId, dbWarnedUserId: warnedUser.dbUserId });
    }
  }

  async warnUser(warningNotification) {
    console.log(warningNotification);
    await fetch(notifyUserRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(warningNotification),
    });
  }

  async updateNote({ warnedUser, ...note }) {
    await fetch(updateNoteRoute, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(note),
    });
    if (note.noteType === 'Warning') {
      await this.warnUser({ dbWarningId: note.noteId, dbWarnedUserId: warnedUser.dbUserId });
    }
  }

  async getNotesByCreatorId(userId) {
    const response = await fetch('http://localhost:8081/api/notes/notes', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(userId.toString()),
    });
    return await response.json();
  }

}