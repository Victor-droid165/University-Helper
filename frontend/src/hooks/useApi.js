import UserService from "../services/UserService";

const userService = new UserService();

export const useApi = () => ({
  validateUserField: async (field, value) => {
    return await userService.validateUserField(field, value);
  },
  validateLogin: async (logInfoSubmission) => {
    return await userService.validateLogin(logInfoSubmission);
  },
  isRegistered: async (user) => {
    return await userService.isRegistered(user);
  },
  registerUser: async (user) => {
    return await userService.registerUser(user);
  },
  getDBUsers: async () => {
    return await userService.getDBUsers();
  },
  updateUserField: async (data) => {
    return await userService.updateUserField(data);
  },
  getUserField: async (data) => {
    return await userService.getUserField(data);
  }
});