import firebase_admin
from firebase_admin import credentials, firestore, auth
import json

# Initialize Firebase with service account (replace 'serviceAccountKey.json' with your file)
cred = credentials.Certificate('serviceAccountKey.json')
firebase_admin.initialize_app(cred)
db = firestore.client()

# Function to authenticate user (additional requirement: user authentication)
def authenticate_user(email, password):
    try:
        user = auth.create_user(email=email, password=password)  # Create user if not exists
        print(f"User {email} authenticated successfully.")
        return user.uid
    except Exception as e:
        print(f"Authentication failed: {e}")
        return None

# Function to insert data (CRUD: insert)
def insert_simulation_result(user_id, result_data):
    doc_ref = db.collection('simulation_results').document(user_id)
    doc_ref.set(result_data)  # Insert key-value data
    print("Simulation result inserted.")

# Function to retrieve data (CRUD: retrieve)
def get_simulation_result(user_id):
    doc_ref = db.collection('simulation_results').document(user_id)
    doc = doc_ref.get()
    if doc.exists:
        return doc.to_dict()
    else:
        print("No data found.")
        return None

# Function to modify data (CRUD: modify)
def update_simulation_result(user_id, updates):
    doc_ref = db.collection('simulation_results').document(user_id)
    doc_ref.update(updates)  # Modify existing data
    print("Simulation result updated.")

# Function to delete data (CRUD: delete)
def delete_simulation_result(user_id):
    doc_ref = db.collection('simulation_results').document(user_id)
    doc_ref.delete()
    print("Simulation result deleted.")

# Example usage (call from R or run standalone)
if __name__ == "__main__":
    user_id = authenticate_user("example@example.com", "password123")
    if user_id:
        # Insert sample data
        sample_data = {"family": "Full", "color": "Black", "percentage": 50.0}
        insert_simulation_result(user_id, sample_data)
        
        # Retrieve
        data = get_simulation_result(user_id)
        print("Retrieved data:", data)
        
        # Update
        update_simulation_result(user_id, {"percentage": 60.0})
        
        # Delete
        delete_simulation_result(user_id)
