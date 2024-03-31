import { capitalize } from "../utils/utils";

const apiUsersURL = process.env.REACT_APP_API + "/users"
const usersValidateRoute = apiUsersURL + "/validate"
const validateRoute = (toValidate) => { return usersValidateRoute + capitalize(toValidate); };
const isRegisteredRoute = apiUsersURL + "/isRegistered";
const registerRoute = apiUsersURL + "/register";
const dbUsersRoute = apiUsersURL + "/usersDB";
const updateUserRoute = apiUsersURL + "/updateAny";
const getUserFieldRoute = apiUsersURL + "/getAny";
export default class UserService {

    async validateUserField(field, value) {
        const response = await fetch(validateRoute(field), {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({ value: value })
        });

        if (!response.ok) {
            throw new Error(`Failed to validate field ${field}.`);
        }

        return await response.json();
    }

    async validateLogin(logInfoSubmission) {
        const response = await fetch(validateRoute("login"), {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(logInfoSubmission)
        });

        if (!response.ok) {
            throw new Error('Failed to validate login.');
        }

        return await response.json();
    }

    async isRegistered(user) {
        const response = await fetch(isRegisteredRoute, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ value: user.email })
        })

        if (!response.ok) {
            throw new Error('Failed to check if the user is registered in our app.');
        }

        return await response.json();
    }

    async registerUser(user) {
        await fetch(registerRoute, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ u_type: user.userType, user: user }),
        });
    }

    async getDBUsers() {
        const response = await fetch(dbUsersRoute, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json',
            }
        });

        if (!response.ok) {
            throw new Error('Failed to fetch users.');
        }

        return await response.json();
    }

    async updateUserField(data) {
        const response = await fetch(updateUserRoute, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify(data),
        });

        if (!response.ok) {
            throw new Error('Failed to update user');
        }

        await response.json();
    }

    async getUserField(data) {
        const queryParams = new URLSearchParams(data).toString();
        const url = getUserFieldRoute + `?${queryParams}`;
        console.log(url);

        const response = await fetch(url, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json',
            }
        });

        if (!response.ok) {
            throw new Error('Failed to fetch user field.');
        }

        return await response.json();
    }

    async getID(idType) {
        const response = await fetch('http://localhost:8081/api/notes/getId', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ value: idType }),
        });
        const data = await response.json();
        console.log(data);
        return data;
    }
      
}